"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const index_1 = require("../node_modules/execa/index");
async function main() {
    console.log("execa:", index_1.execa);
    for await (const line of (0, index_1.execa)({ shell: true }) `pnpm add three`) {
        console.log("line:", line);
    }
    console.log("done");
}
main().catch(console.error);
