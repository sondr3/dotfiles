import { Args, parse } from "flags/mod.ts";
import { expandGlob } from "fs/mod.ts";
import { Context, Group, status } from "./mod.ts";

const HELP_MESSAGE = `dots utility v0.1

A small utility to help me with managing dotfiles.

USAGE:
  dots

COMMANDS:
  list, l           List all tasks/groups
  build, b          Build dotfiles
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

    window.context = new Context(this.argv.verbose);
  }

  printHelp() {
    console.log(HELP_MESSAGE);
    return Deno.exit();
  }

  async *walkdir() {
    for await (const entry of expandGlob("**/*.task.ts")) {
      yield entry;
    }
  }

  async *groups() {
    for await (const entry of this.walkdir()) {
      const { default: module }: { default: Group } = await import(entry.path);
      yield module;
    }
  }

  async execute() {
    switch (this.argv._[0]) {
      case "h":
      case "help": {
        return this.printHelp();
      }
      case "l":
      case "list": {
        window.context.info = true;
        for await (const group of this.groups()) {
          console.dir(group);
          console.log(`${group.name}: ${group.description}`);
        }
        break;
      }
      case "b":
      case "build": {
        for await (const module of this.groups()) {
          module.tasks.forEach(async (task) => {
            await task.cb();
          });
        }
        break;
      }
      case "s":
      case "status": {
        status();
        break;
      }
      default: {
        return this.printHelp();
      }
    }
  }
}
