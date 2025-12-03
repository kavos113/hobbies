const fs = require("fs");
const path = require("path");
const Parser = require("tree-sitter");
const { typescript } = require("tree-sitter-typescript");

const q1 = `(function_signature
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

const q2 = `(method_signature
  name: (property_identifier) @name.definition.method) @definition.method
`

const parser = new Parser();
parser.setLanguage(typescript);
const query = new Parser.Query(
  typescript,
  q1
);

const filename = "example.ts";
const code = fs.readFileSync(
  filename,
  "utf8"
);
const tree = parser.parse(code);
const captures = query.captures(tree.rootNode);
const lines = code.split("\n");
let lastLine = -1;

captures.sort((a, b) => {
  a.node.startPosition.row - b.node.startPosition.row;
});
captures.forEach((capture) => {
  const node = capture.node;
  const name = capture.name;
  const type = capture.type;
  const startPosition = node.startPosition;
  const endPosition = node.endPosition;

  console.log(
    `${startPosition.row} ${lines[startPosition.row]}`
  );
});