export async function task(
  name: string,
  description: string,
  cb: () => Promise<void>
) {
  console.log(`${name}`);

  await cb();
}
