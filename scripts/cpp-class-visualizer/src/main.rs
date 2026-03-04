use once_cell::sync::Lazy;
use regex::Regex;
use std::{env, fs};
use walkdir::WalkDir;

#[derive(Debug, Clone)]
struct Class {
    name: String,
    super_class: Option<String>,
    access_modifier: Option<String>,
}

fn main() {
    let args = env::args().collect::<Vec<String>>();
    if args.len() < 2 {
        println!("Usage: {} <dir> [exclude dirs]", args[0]);
        return;
    }

    let cpp_exts = ["cpp", "h", "hpp", "cc", "cxx", "hh", "hxx"];

    let dirname = &args[1];
    let exclude_dirs: Vec<String> = if args.len() > 2 {
        args[2..].to_vec()
    } else {
        Vec::new()
    };
    let mut extends_list: Vec<Class> = Vec::new();

    for entry in WalkDir::new(dirname)
        .into_iter()
        .filter_entry(|e| {
            !e.file_name()
                .to_str()
                .map(|s| exclude_dirs.iter().any(|ex| s.contains(ex)))
                .unwrap_or(false)
        })
        .filter_map(|e| e.ok())
    {
        if entry.file_type().is_file() {
            if let Some(ext) = entry.path().extension() {
                if cpp_exts.contains(&ext.to_str().unwrap_or_default()) {
                    let content = fs::read_to_string(entry.path()).expect("Failed to read file");
                    let mut classes = read_file(&*strip_comment(&content));
                    extends_list.append(&mut classes);
                }
            }
        }
    }

    static KEYWORDS_RE: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r##"\b(class|struct|public|private|protected|virtual|override|final|abstract)\b"##).unwrap()
    });

    extends_list = extends_list
        .into_iter()
        .filter(|c| {
            !KEYWORDS_RE.is_match(&c.name)
                && if let Some(super_class) = &c.super_class {
                    !KEYWORDS_RE.is_match(super_class)
                } else {
                    true
                }
        })
        .filter(|c| c.name != ":")
        .collect();

    static SUPER_FILTER_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r##"u?int(\d)+_t"##).unwrap());
    extends_list.retain(|c| {
        if let Some(super_class) = &c.super_class {
            !SUPER_FILTER_RE.is_match(super_class)
        } else {
            true
        }
    });

    let uml = generate_uml(extends_list);
    fs::write("output.pu", uml).expect("Failed to write UML file");
}

fn strip_comment(text: &str) -> String {
    static COMMENT_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r##"(?s)/\*.*?\*/|//[^\n]*"##).unwrap());
    COMMENT_RE.replace_all(text, "").to_string()
}

fn read_file(content: &str) -> Vec<Class> {
    static CLASS_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r##"(?m)\bclass\s+([^:{;]+?)\s*\{"##).unwrap());
    static CLASS_EXTENDS_RE: Lazy<Regex> =
        Lazy::new(|| Regex::new(r##"(?m)\bclass\s+([^:{;]+?)\s*:([^{;]*)\{"##).unwrap());

    let mut extends_list: Vec<Class> = Vec::new();

    for mat in CLASS_EXTENDS_RE.captures_iter(content) {
        let raw_class_name = mat.get(1).unwrap().as_str();
        let class_name = clean_class_name(raw_class_name);
        let extends_str = mat.get(2).unwrap().as_str();

        let ex_block = split_extends(extends_str);

        for ex in ex_block {
            let (modifier, super_class) = parse_super_class(&ex);
            extends_list.push(Class {
                super_class: Some(super_class),
                name: class_name.clone(),
                access_modifier: modifier,
            });
        }
    }

    for mat in CLASS_RE.captures_iter(content) {
        let raw_class_name = mat.get(1).unwrap().as_str();
        let class_name = clean_class_name(raw_class_name);
        if !extends_list.iter().any(|c| c.name == class_name) {
            extends_list.push(Class {
                super_class: None,
                name: class_name,
                access_modifier: None,
            });
        }
    }

    extends_list
}

fn clean_class_name(s: &str) -> String {
    s.replace("final", "").split_whitespace().collect::<Vec<&str>>().join(" ")
}

fn split_extends(s: &str) -> Vec<String> {
    let mut result = Vec::new();
    let mut current = String::new();
    let mut depth = 0;

    for c in s.chars() {
        match c {
            '<' => { depth += 1; current.push(c); }
            '>' => { depth -= 1; current.push(c); }
            ',' if depth == 0 => {
                result.push(current.trim().to_string());
                current.clear();
            }
            _ => { current.push(c); }
        }
    }
    let trimmed = current.trim();
    if !trimmed.is_empty() {
        result.push(trimmed.to_string());
    }
    result
}

fn parse_super_class(s: &str) -> (Option<String>, String) {
    let parts: Vec<&str> = s.split_whitespace().collect();
    let mut modifiers = Vec::new();
    let mut class_name_parts = Vec::new();

    let keywords = ["public", "protected", "private", "virtual"];

    for part in parts {
        if keywords.contains(&part) {
            modifiers.push(part.to_string());
        } else {
            class_name_parts.push(part.to_string());
        }
    }

    let modifier = if modifiers.is_empty() {
        None
    } else {
        Some(modifiers.join(" "))
    };

    (modifier, class_name_parts.join(" "))
}

fn generate_uml(classes: Vec<Class>) -> String {
    let mut uml = String::new();

    uml += "@startuml output.png\n";

    for class in &classes {
        uml += &format!("class {} {{\n}}\n", class.name);
    }

    for class in &classes {
        if let Some(super_class) = &class.super_class {
            uml += &format!("{} <|-- {}\n", super_class, class.name);
        }
    }

    uml
}
