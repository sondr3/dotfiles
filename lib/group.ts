type Group = "install" | "files";

export async function group(
  name: string,
  description: string,
  cb: () => Promise<void> | void
) {
  if (!window.context.verbose) {
    console.log(`${name}`);
  } else {
    console.log(`${name}: ${description}`);
  }

  await cb();
}
