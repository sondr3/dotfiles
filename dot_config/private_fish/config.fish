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

# latex
test -d /usr/local/texlive/2022/bin/x86_64-linux; and fish_add_path /usr/local/texlive/2022/bin/x86_64-linux 
set PATH (string match -v /usr/local/texlive/2021/bin/x86_64-linux $PATH) # FIXME

# 1Password SSH 
set SSH_AUTH_SOCK $HOME/.1password/agent.sock

# homebrew

if test -d /opt/homebrew
  /opt/homebrew/bin/brew shellenv | source
  fish_add_path /opt/homebrew/bin
  set -gx HOMEBREW_NO_ENV_HINTS 1
end

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

test -d $HOME/.local/bin; and fish_add_path $HOME/.local/bin 

function ..; cd ..; end
function ...; cd ../..; end
function ....; cd ../../..; end

# Abbrevations

abbr -a -g c chezmoi
abbr -a -g g git
