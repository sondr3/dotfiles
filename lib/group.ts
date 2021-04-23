type Group = "install" | "files";

export async function group(
  name: string,
  description: string,
  cb: () => Promise<void> | void,
) {
  console.log(`${name}`);

  await cb();
}