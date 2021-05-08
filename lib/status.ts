export async function status() {
  const status = Deno.run({
    cmd: ["git", "status", "-s"],
    cwd: `${Deno.env.get("HOME")}/.dotfiles/`,
  });

  await status.status();
}
