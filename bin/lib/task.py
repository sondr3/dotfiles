from abc import ABC, abstractmethod
from dataclasses import dataclass
from logging import Logger, getLogger


@dataclass
class Task(ABC):
    name: str
    description: str = None
    logger: Logger = getLogger(__name__)

    @abstractmethod
    def tasks(self) -> None:
        pass
