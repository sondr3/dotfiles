# disable greeting
set -U fish_greeting

# Configure paths for PNPM
set -gx PNPM_HOME "/home/sondre/.local/share/pnpm"
fish_add_path "$PNPM_HOME"

# ghcup-env
set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME
test -d $HOME/.ghcup/bin ; and fish_add_path $HOME/.cabal/bin $HOME/.ghcup/bin

# deno
test -f $HOME/.deno/bin/deno ; and fish_add_path $HOME/.deno/bin 

# rust
test -d $HOME/.cargo/bin; and fish_add_path $HOME/.cargo/bin 

# go
test -d /usr/local/go/bin; and fish_add_path /usr/local/go/bin/ 

# python
type -q python3; and fish_add_path (python3 -m site --user-base)/bin

# ~/.local/bin
test -d $HOME/.local/bin; and fish_add_path $HOME/.local/bin 

# 1Password SSH 
set SSH_AUTH_SOCK $HOME/.1password/agent.sock

# homebrew
if test -d /opt/homebrew
  /opt/homebrew/bin/brew shellenv | source
  fish_add_path /opt/homebrew/bin
  set -gx HOMEBREW_NO_ENV_HINTS 1
end

# configure applications
fnm env --shell fish --use-on-cd | source
direnv hook fish | source
starship init fish | source
zoxide init fish | source
op completion fish | source

# aliases
alias l="ls -la"
alias fzp="fzf --preview 'bat --style=numbers --color=always {}'"

# variables
set -gx EDITOR nvim
set FZF_ALT_C_COMMAND "fd --type d"
set FZF_DEFAULT_COMMAND "fd --type f"

# functions
function ..; cd ..; end
function ...; cd ../..; end
function ....; cd ../../..; end

# Abbrevations
abbr -a -g g git
