import * as path from "path/mod.ts";
import { walk } from "./fs.ts";
import { exists } from "fs/mod.ts";

export enum Shell {
  Bash,
  Fish,
}

export function getShell() {
  const shell = Deno.env.get("SHELL");
  if (!shell) return Shell.Bash;

  switch (path.basename(shell)) {
    case "fish":
      return Shell.Fish;
    case "bash":
      return Shell.Bash;
    default:
      return Shell.Bash;
  }
}

export function getPath() {
  const shell = getShell();
  const sep = shell === Shell.Bash ? ":" : " ";
  const path = Deno.env.get("PATH");

  if (!path) throw new Error("Could not get $PATH");
  const paths = path.split(sep);
  return paths;
}

export async function which(executable: string) {
  const paths = getPath().reverse();
  for (const dir of paths) {
    if (!await exists(dir)) continue;
    const dirWalk = walk(dir);
    for await (const file of dirWalk) {
      if (file.name === executable) {
        dirWalk.return();
        return file;
      }
    }
  }
}
