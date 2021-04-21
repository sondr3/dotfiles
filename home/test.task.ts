import { group, task } from "../lib/mod.ts";

group("Test", "A test task", () => {
  task("create something", "what even is this", async () => {
    await Deno.open("config.dot");
    console.log(window.context);

    console.log("Hello from Test!");
  });
});
