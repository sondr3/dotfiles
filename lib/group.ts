import { Task } from "./mod.ts";

type GroupType = "install" | "files";

export interface Group {
  name: string;
  description: string;
  tasks: ReadonlyArray<Task>;
}

export function group(
  name: string,
  description: string,
  tasks: ReadonlyArray<Task>,
): Group {
  return {
    name,
    description,
    tasks,
  };
}
