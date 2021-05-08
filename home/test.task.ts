import { group, symlink, task, taskDirectory } from "dots";

export default group("test", "a test group", [
  task("thing", "copy some stuff", async () => {
    const dirname = taskDirectory(import.meta.url);
    await symlink(
      `${dirname}/kitty.conf`,
      `/home/sondre/.config/kitty/new.conf`,
    );
  }),
]);
