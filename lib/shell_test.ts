import { assert, assertEquals } from "testing/asserts.ts";
import { getPath, getShell, Shell } from "./shell.ts";

Deno.test("getShell - fish", () => {
  Deno.env.set("SHELL", "/usr/bin/fish");
  assertEquals(getShell(), Shell.Fish);
});

Deno.test("getShell - bash", () => {
  Deno.env.set("SHELL", "/usr/bin/bash");
  assertEquals(getShell(), Shell.Bash);
});

Deno.test("getShell - undefined", () => {
  Deno.env.delete("SHELL");
  assertEquals(getShell(), Shell.Bash);
});

Deno.test("getPath", () => {
  const paths = getPath();
  assert(paths.length > 0);
});
