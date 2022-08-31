# disable greeting
set -U fish_greeting

# ghcup-env
test -d $HOME/.ghcup/bin ; and fish_add_path $HOME/.cabal/bin $HOME/.ghcup/bin

# deno
test -f $HOME/.deno/bin/deno ; and fish_add_path $HOME/.deno/bin 

# rust
test -d $HOME/.cargo/bin; and fish_add_path $HOME/.cargo/bin 

# go
test -d /usr/local/go/bin; and fish_add_path /usr/local/go/bin/ 

# python
type -q python3; and fish_add_path $HOME/Library/Python/3.10/bin/

# ~/.local/bin
test -d $HOME/.local/bin; and fish_add_path $HOME/.local/bin 

# 1Password SSH 
set SSH_AUTH_SOCK $HOME/.1password/agent.sock

# homebrew
if test -d /opt/homebrew
  set -gx HOMEBREW_PREFIX "/opt/homebrew";
  set -gx HOMEBREW_CELLAR "/opt/homebrew/Cellar";
  set -gx HOMEBREW_REPOSITORY "/opt/homebrew";
  set -q PATH; or set PATH ''; set -gx PATH "/opt/homebrew/bin" "/opt/homebrew/sbin" $PATH;
  set -q MANPATH; or set MANPATH ''; set -gx MANPATH "/opt/homebrew/share/man" $MANPATH;
  set -q INFOPATH; or set INFOPATH ''; set -gx INFOPATH "/opt/homebrew/share/info" $INFOPATH;
  fish_add_path /opt/homebrew/bin
  set -gx HOMEBREW_NO_ENV_HINTS 1
end

# configure applications
starship init fish --print-full-init | source
fnm env --shell fish --use-on-cd | source
direnv hook fish | source
zoxide init fish | source

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
abbr -a -g py python3
abbr -a -g pn pnpm
