import { group, symlink, task } from "dots";
import filedirname from "filedirname";

export default group("test", "a test group", [
  task("thing", "copy some stuff", async () => {
    const [__filename, __dirname] = filedirname();
    await symlink(
      `${__dirname}/kitty.conf`,
      `/home/sondre/.config/kitty/new.conf`,
    );
  }),
]);
