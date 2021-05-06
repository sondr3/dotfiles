import { Args, parse } from "flags/mod.ts";
import { expandGlob } from "fs/mod.ts";
import { buildContext } from "./mod.ts";

const HELP_MESSAGE = `dots utility v0.1

A small utility to help me with managing dotfiles.

USAGE:
  dots

COMMANDS:
  help, h           Show help

OPTIONS:
  -v, --verbose     Verbose output
`;

export class CLI {
  private argv: Args;

  constructor() {
    this.argv = parse(Deno.args, {
      boolean: ["verbose"],
      default: { verbose: false },
      alias: { v: "verbose" },
    });

    window.context = buildContext(this.argv.verbose);
  }

  printHelp() {
    console.log(HELP_MESSAGE);
    return Deno.exit();
  }

  async execute() {
    switch (this.argv._[0]) {
      case "h":
      case "help": {
        return this.printHelp();
      }
      case "build": {
        for await (const entry of expandGlob("**/*.task.ts")) {
          await import(entry.path);
        }
        break;
      }
      default: {
        return this.printHelp();
      }
    }
  }
}
