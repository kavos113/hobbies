use regex::Regex;
use std::{env, fs};

struct Class {
    name: String,
    super_class: Option<String>,
    access_modifier: Option<String>,
}

fn main() {
    let args = env::args().collect::<Vec<String>>();
    if args.len() < 2 {
        println!("Usage: {} <file>", args[0]);
        return;
    }

    let filename = &args[1];

    let class_re = Regex::new(r##"^(?m)\s*class\s+(\S+)\s*\{"##).unwrap();
    let class_extends_re = Regex::new(r##"(?m)^\s*class\s+(\S+)\s*:([^{]*)\{"##).unwrap();

    let content = fs::read_to_string(filename).expect("Failed to read file");

    let mut extends_list: Vec<Class> = Vec::new();

    for mat in class_extends_re.captures_iter(&content) {
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

    for mat in class_re.captures_iter(&content) {
        let class_name = mat.get(1).unwrap().as_str();
        if !extends_list.iter().any(|c| c.name == class_name) {
            extends_list.push(Class {
                super_class: None,
                name: class_name.to_string(),
                access_modifier: None,
            });
        }
    }

    let uml = generate_uml(extends_list);
    fs::write("output.pu", uml).expect("Failed to write UML file");
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