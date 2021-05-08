import { ensureSymlink, walk as _walk } from "fs/mod.ts";
import { fileURLToPath } from "node/url.ts";
import * as path from "path/mod.ts";

export function taskFileName(url: string) {
  return fileURLToPath(url);
}

export function taskDirectory(url: string) {
  const filename = fileURLToPath(url);
  return path.dirname(filename);
}

export async function createSymlink(from: string, to: string) {
  try {
    await ensureSymlink(from, to);
  } catch (error) {
    console.error(`Could not create symlink: ${error}`);
    Deno.exit(1);
  }
}

export async function mkdirp(dir: string) {
  try {
    await Deno.mkdir(dir, { recursive: true });
  } catch (error) {
    throw new Error(`Could not create directory: ${error}`);
  }
}

export async function* walk(directory: string) {
  for await (const file of _walk(directory)) {
    yield file;
  }
}
