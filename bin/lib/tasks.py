import importlib.util
import importlib
from task import Task
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


def run_tasks(paths: List[Path]):
    for path in paths:
        with open(path, "r") as f:
            content = f.read()
            print(content)
            task = eval(content, globals(), {Task.__name__: Task})
            task.tasks()
        # importlib.import_module(path.stem)
        # spec = importlib.util.spec_from_file_location(path.stem, path)
        # module = importlib.util.module_from_spec(spec)
        # sys.modules[path.stem] = module
        #
        # print(module.__name__)
        #
        # import test.task as t
        # t.tasks()
