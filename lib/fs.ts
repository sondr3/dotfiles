import { ensureSymlink } from "fs/mod.ts";
import { fileURLToPath } from "node/url.ts";
import * as path from "path/mod.ts";

export function taskDirectory(url: string) {
  const filename = fileURLToPath(url);
  return path.dirname(filename);
}

export async function createSymlink(from: string, to: string) {
  await ensureSymlink(from, to);
}

export async function mkdirp(dir: string) {
  try {
    await Deno.mkdir(dir, { recursive: true });
  } catch (error) {
    throw new Error(`Could not create directory: ${error}`);
  }
}
