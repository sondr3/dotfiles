import { walk, expandGlob } from "fs/mod.ts";
import { Context, buildContext } from "./lib/mod.ts";
import { parse } from "flags/mod.ts";

const help = `dots utility v0.1

A small utility to help me with managing dotfiles.

USAGE:
  dots
OPTIONS:
  -h, --help        Show this message
`;

async function main() {
  const argv = parse(Deno.args, {
    boolean: ["help"],
    default: { verbose: false, help: false },
    alias: { h: "help" },
  });

  if (argv.help) {
    console.log(help);
    Deno.exit();
  }

  window.context = buildContext(argv.verbose);

  for await (const entry of expandGlob("**/*.task.ts")) {
    const file = await import(entry.path);
  }
}

void main();
