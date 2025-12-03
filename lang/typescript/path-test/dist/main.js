"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const globby_1 = require("globby");
const entries = (0, globby_1.globby)("*", {
    cwd: process.cwd(),
    onlyFiles: false,
    dot: true,
    markDirectories: true,
    absolute: false,
}).then((entries) => {
    console.log(entries);
});
