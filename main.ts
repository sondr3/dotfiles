import { walk, expandGlob } from "fs/mod.ts";

console.log("Hello, world!");

export default async function main() {
  for await (const entry of expandGlob("**/*.task.ts")) {
    const file = await import(entry.path);
  }
}

void main();
