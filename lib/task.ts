export async function task(
  name: string,
  description: string,
  cb: () => Promise<void> | void,
) {
  console.log(`${name}`);

  await cb();
}
