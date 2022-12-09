# disable greeting
set -U fish_greeting
set -l os (uname)

# ghcup-env
test -d $HOME/.ghcup/bin ; and fish_add_path $HOME/.cabal/bin $HOME/.ghcup/bin

# deno
test -f $HOME/.deno/bin/deno ; and fish_add_path $HOME/.deno/bin 

# rust
test -d $HOME/.cargo/bin; and fish_add_path $HOME/.cargo/bin 

# go
test -d /usr/local/go/bin; and fish_add_path /usr/local/go/bin/ 

# dotnet
if test -d $HOME/.dotnet 
  set -gx DOTNET_ROOT "$HOME/.dotnet"
  fish_add_path $HOME/.dotnet
  fish_add_path $HOME/.dotnet/tools/
end

# python
if type -q python3; and test "$os" = "Darwin"
  fish_add_path $HOME/Library/Python/3.10/bin/
  fish_add_path /opt/homebrew/opt/python@3.10/libexec/bin
end

# pnpm
if test "$os" = "Darwin"
  set -gx PNPM_HOME "/Users/sondre/Library/pnpm"
else
  set -gx PNPM_HOME "$HOME/.local/share/pnpm"
end
set -gx PATH "$PNPM_HOME" $PATH

# ~/.local/bin
test -d $HOME/.local/bin; and fish_add_path $HOME/.local/bin 

# 1Password SSH 
set SSH_AUTH_SOCK $HOME/.1password/agent.sock

# homebrew
if test -d /opt/homebrew; and test (arch) = "arm64"; and test "$os" = "Darwin"
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

if test (arch) = "i386"; and test "$os" = "Darwin"
  echo "USING x86 ROSETTA TERMINAL!!!"
  eval /usr/local/bin/brew shellenv | source
  # >>> conda initialize >>>
  # !! Contents within this block are managed by 'conda init' !!
  eval /opt/homebrew/Caskroom/miniconda/base/bin/conda "shell.fish" "hook" $argv | source
  # <<< conda initialize <<<
end
