set -U fish_greeting

# Configure paths for PNPM
set -gx PNPM_HOME "/home/sondre/.local/share/pnpm"
set -gx PATH "$PNPM_HOME" $PATH

# ghcup-env
set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME
test -d $HOME/.ghcup/bin ; and set -gx PATH $HOME/.cabal/bin $HOME/.ghcup/bin $PATH

# deno
test -f $HOME/.deno/bin/deno ; and set -gx PATH $HOME/.deno/bin $PATH

# rust
test -d $HOME/.cargo/bin; and set -gx PATH $HOME/.cargo/bin $PATH

# latex
test -d /usr/local/texlive/2021/bin/x86_64-linux; and set -gx PATH /usr/local/texlive/2021/bin/x86_64-linux $PATH

# Configure applications

fnm env --shell fish --use-on-cd | source
direnv hook fish | source
starship init fish | source
zoxide init fish | source

alias l="ls -la"
alias fzp="fzf --preview 'bat --style=numbers --color=always {}'"

set -gx EDITOR nvim

set FZF_ALT_C_COMMAND "fd --type d"
set FZF_DEFAULT_COMMAND "fd --type f"

test -d $HOME/.local/bin; and set -gx PATH $HOME/.local/bin $PATH

function ..; cd ..; end
function ...; cd ../..; end
function ....; cd ../../..; end

# Abbrevations

abbr -a -g c chezmoi
abbr -a -g g git

# SSH
eval (ssh-agent -c) > /dev/null
