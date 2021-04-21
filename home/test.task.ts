import { group, task } from "../lib/mod.ts";

group("Test", "A test task", async () => {
  task("create something", "what even is this", async () => {
    console.log(window.context);

    console.log("Hello from Test!");
  });
});
