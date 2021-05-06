export async function task(
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
