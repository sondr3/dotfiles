import { createSymlink } from "./fs.ts";

export interface Task {
  name: string;
  description: string;
  cb: () => Promise<void> | void;
}

export async function symlink(from: string, to: string) {
  if (window.context.verbose) {
    console.info(`Creating symlink ${from} -> ${to}`);
  }
  await createSymlink(from, to);
}

export function task(
  name: string,
  description: string,
  cb: () => Promise<void> | void,
): Task {
  return {
    name,
    description,
    cb,
  };
}
