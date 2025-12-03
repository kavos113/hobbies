"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || (function () {
    var ownKeys = function(o) {
        ownKeys = Object.getOwnPropertyNames || function (o) {
            var ar = [];
            for (var k in o) if (Object.prototype.hasOwnProperty.call(o, k)) ar[ar.length] = k;
            return ar;
        };
        return ownKeys(o);
    };
    return function (mod) {
        if (mod && mod.__esModule) return mod;
        var result = {};
        if (mod != null) for (var k = ownKeys(mod), i = 0; i < k.length; i++) if (k[i] !== "default") __createBinding(result, mod, k[i]);
        __setModuleDefault(result, mod);
        return result;
    };
})();
Object.defineProperty(exports, "__esModule", { value: true });
const diff = __importStar(require("diff"));
const oldStr = "hello world\nsome text\nmore text\n\nmoremore text\nhenka nashi\nhenka ari\n";
const newStr = "hello world\nsome more text\nmore text\n\nmoremore text\nhenka nashi\nhenka arisssss\n";
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
