import { group, task } from "dots";

group("test", "a test group", () => {
  task("thing", "copy some stuff", () => {
    console.log(window.context);
    console.log(["This", "is", "a", "task"]);
  });
});
