set -U fish_greeting

# Configure applications

direnv hook fish | source
starship init fish | source
zoxide init fish | source

alias l="ls -la"
alias fzp="fzf --preview 'bat --style=numbers --color=always {}'"

set -gx EDITOR nvim

set FZF_ALT_C_COMMAND "fd --type d"
set FZF_DEFAULT_COMMAND "fd --type f"
set NVM_SYMLINK_CURRENT "true"

test -d $HOME/.local/bin; and set -gx PATH $HOME/.local/bin $PATH

function ..; cd ..; end
function ...; cd ../..; end
function ....; cd ../../..; end

# SSH
eval (ssh-agent -c) > /dev/null

# ghcup-env
set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME
test -f /home/sondre/.ghcup/env ; and set -gx PATH $HOME/.cabal/bin /home/sondre/.ghcup/bin $PATH

# deno
test -f $HOME/.deno/bin/deno ; and set -gx PATH $HOME/.deno/bin $PATH

# rust
test -d $HOME/.cargo/bin; and set -gx PATH $HOME/.cargo/bin $PATH
