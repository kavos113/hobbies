import { execa } from "../node_modules/execa/index";

async function main() {
    console.log("execa:", execa);
    for await (const line of execa({ shell: true })`pnpm add three`) {
        console.log("line:", line);
    }
    console.log("done");
}

main().catch(console.error);
