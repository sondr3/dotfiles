from pathlib import Path
from typing import List

IGNORED_FOLDERS = [".git", ".idea", ".vscode", "bin"]
IGNORED_FILES = [".gitignore"]


def find_tasks(path: Path, tasks=None) -> List[Path]:
    if tasks is None:
        tasks = list()

    for file in path.iterdir():
        if file.name in IGNORED_FOLDERS or file.name in IGNORED_FILES:
            continue
        elif file.is_dir():
            find_tasks(file, tasks)
        elif file.name.endswith(".task.py"):
            tasks.append(file)

    return tasks
