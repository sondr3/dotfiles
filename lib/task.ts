export interface Task {
  name: string;
  description: string;
  cb: () => Promise<void> | void;
}

export function task(
  name: string,
  description: string,
  cb: () => Promise<void> | void,
): Task {
  return {
    name,
    description,
    cb,
  };
}
