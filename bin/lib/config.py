import json
import os
from configparser import ConfigParser, Error
from dataclasses import dataclass


@dataclass
class Config:
    name: str = None
    email: str = None
    home: str = os.environ.get("HOME")
    root: str = os.path.join(home, ".dotfiles/")
    config: ConfigParser = ConfigParser()

    def read_config(self) -> bool:
        with open(os.path.join(self.root, "config.dot"), "r") as f:
            self.config.read_file(f)

            try:
                self.name = self.config["User"]["name"]
                self.email = self.config["User"]["email"]
            except KeyError as e:
                print(f"`{e}` not found in `config.dot`")
                exit(1)

        return True
