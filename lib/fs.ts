import { ensureSymlink } from "fs/mod.ts";

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
