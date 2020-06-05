if not functions -q fisher
    set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
    curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
    fish -c fisher
end

direnv hook fish | source
starship init fish | source
zoxide init --cmd j fish | source

alias l="ls -la"
alias fzp="fzf --preview 'bat --style=numbers --color=always {}'"

set FZF_ALT_C_COMMAND "fd --type d"
set FZF_DEFAULT_COMMAND "fd --type f"
