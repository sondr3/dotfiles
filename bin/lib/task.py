from abc import ABC, abstractmethod
from dataclasses import dataclass


@dataclass
class Task(ABC):
    name: str
    description: str = None

    @abstractmethod
    def tasks(self) -> None:
        pass
