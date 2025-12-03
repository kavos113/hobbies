import * as diff from "diff";

const oldStr =
  "hello world\nsome text\nmore text\n\nmoremore text\nhenka nashi\nhenka ari\n";
const newStr =
  "hello world\nsome more text\nmore text\n\nmoremore text\nhenka nashi\nhenka arisssss\n";

const diffResult = diff.createPatch("file.txt", oldStr, newStr);
console.log(diffResult);

const diffLines = diff.diffLines(oldStr, newStr);
console.log(diffLines);

const diffRepresentation = diff
  .diffLines(oldStr, newStr)
  .map((part) => {
    const prefix = part.added ? "+" : part.removed ? "-" : " ";
    return (part.value || "")
      .split("\n")
      .map((line) => (line ? prefix + line : ""))
      .join("\n");
  })
  .join("");

console.log(diffRepresentation);