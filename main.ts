import { walk, expandGlob } from "fs/mod.ts";
import { Context, defaultContext } from "./lib/mod.ts";

window.context = defaultContext();

export default async function main() {
  for await (const entry of expandGlob("**/*.task.ts")) {
    const file = await import(entry.path);
  }
}

void main();
