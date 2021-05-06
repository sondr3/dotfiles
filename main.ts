import { expandGlob } from "fs/mod.ts";
import { buildContext } from "./lib/mod.ts";
import { parse } from "flags/mod.ts";

const help = `dots utility v0.1

A small utility to help me with managing dotfiles.

USAGE:
  dots

COMMANDS:
  help, h           Show help

OPTIONS:
  -v, --verbose     Verbose output
`;

async function main() {
  const argv = parse(Deno.args, {
    boolean: ["verbose"],
    default: { verbose: false },
    alias: { v: "verbose" },
  });

  switch (argv._[0]) {
    case "h":
    case "help": {
      console.log(help);
      return Deno.exit();
    }
    case "build": {
      window.context = buildContext(argv.verbose);

      for await (const entry of expandGlob("**/*.task.ts")) {
        await import(entry.path);
      }
      break;
    }
    default: {
      console.log(help);
      return Deno.exit();
    }
  }
}

void main();
