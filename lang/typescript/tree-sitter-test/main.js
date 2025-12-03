const fs = require("fs");
const path = require("path");
const Parser = require("tree-sitter");
const { typescript } = require("tree-sitter-typescript");

const dir = process.argv[2];
if (!dir) {
  console.error("Directory argument missing");
  process.exit(1);
}

const parser = new Parser();
parser.setLanguage(typescript);

const query = new Parser.Query(
  typescript,
  `
(function_signature
  name: (identifier) @name.definition.function) @definition.function

(method_signature
  name: (property_identifier) @name.definition.method) @definition.method

(abstract_method_signature
  name: (property_identifier) @name.definition.method) @definition.method

(abstract_class_declaration
  name: (type_identifier) @name.definition.class) @definition.class

(module
  name: (identifier) @name.definition.module) @definition.module

(interface_declaration
  name: (type_identifier) @name.definition.interface) @definition.interface

(function_declaration
  name: (identifier) @name.definition.function) @definition.function

(method_definition
  name: (property_identifier) @name.definition.method) @definition.method

(class_declaration
  name: (type_identifier) @name.definition.class) @definition.class

(type_alias_declaration
  name: (type_identifier) @name.definition.type) @definition.type

(enum_declaration
  name: (identifier) @name.definition.enum) @definition.enum
`
);

function processDirectory(directory) {
  const items = fs.readdirSync(directory);
  items.forEach((item) => {
    const itemPath = path.join(directory, item);
    const stats = fs.statSync(itemPath);
    if (stats.isDirectory()) {
      processDirectory(itemPath);
    } else {
      const code = fs.readFileSync(itemPath, "utf8");
      let formattedOutput = "";
      try {
        const tree = parser.parse(code);
        const captures = query.captures(tree.rootNode);
        const lines = code.split("\n");
        let lastLine = -1;

        captures.sort(
          (a, b) => a.node.startPosition.row - b.node.startPosition.row
        );
        captures.forEach((capture) => {
          const { node, name } = capture;
          const startLine = node.startPosition.row;
          const endLine = node.endPosition.row;
          console.log(endLine);

          if (lastLine !== -1 && startLine > lastLine + 1) {
            formattedOutput += "|----\n";
          }
          if (name.includes("name") && lines[startLine]) {
            formattedOutput += `│${lines[startLine]}\n`;
          }
          lastLine = endLine;
        });

        if (formattedOutput.length > 0) {
          const relativePath = path.relative(dir, itemPath);
          console.log(relativePath);
          console.log(`|----\n${formattedOutput}|----\n`);
        }
      } catch (error) {
        console.log(`Error parsing file: ${error}\n`);
      }
    }
  });
}

processDirectory(dir);
