import { globby } from "globby";

const entries = globby("*", {
    cwd: process.cwd(),
    onlyFiles: false,
    dot: true,
    markDirectories: true,
    absolute: false,
}).then((entries) => {
    console.log(entries);
});