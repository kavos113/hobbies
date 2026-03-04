use once_cell::sync::Lazy;
use regex::Regex;
use std::{env, fs};
use walkdir::WalkDir;

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
        println!("Processing: {}", entry.path().display());

        if entry.file_type().is_file() {
            if let Some(ext) = entry.path().extension() {
                if cpp_exts.contains(&ext.to_str().unwrap_or_default()) {
                    let content = fs::read_to_string(entry.path()).expect("Failed to read file");
                    let mut classes = read_file(content);
                    extends_list.append(&mut classes);
                }
            }
        }
    }

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

fn read_file(content: String) -> Vec<Class> {
    static CLASS_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r##"\s*class\s+(\S+)\s*\{"##).unwrap());
    static CLASS_EXTENDS_RE: Lazy<Regex> =
        Lazy::new(|| Regex::new(r##"\s*class\s+(\S+)\s*:([^{]*)\{"##).unwrap());

    let mut extends_list: Vec<Class> = Vec::new();

    for mat in CLASS_EXTENDS_RE.captures_iter(&content) {
        let class_name = mat.get(1).unwrap().as_str();
        let extends = mat.get(2).unwrap().as_str();

        let ex_block = extends.split(",").map(|s| s.trim()).collect::<Vec<&str>>();
        for ex in ex_block {
            let b = ex.split_whitespace().collect::<Vec<&str>>();
            if b.len() == 2 {
                extends_list.push(Class {
                    super_class: Some(b[1].to_string()),
                    name: class_name.to_string(),
                    access_modifier: Some(b[0].to_string()),
                });
            } else {
                extends_list.push(Class {
                    super_class: Some(ex.to_string()),
                    name: class_name.to_string(),
                    access_modifier: None,
                });
            }
        }
    }

    for mat in CLASS_RE.captures_iter(&content) {
        let class_name = mat.get(1).unwrap().as_str();
        if !extends_list.iter().any(|c| c.name == class_name) {
            extends_list.push(Class {
                super_class: None,
                name: class_name.to_string(),
                access_modifier: None,
            });
        }
    }

    extends_list
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
