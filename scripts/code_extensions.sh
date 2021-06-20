#!/usr/bin/env bash

cat ~/.config/Code/User/extensions.list | xargs -L 1 code --install-extension
