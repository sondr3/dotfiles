# Remove greeting
set -g -x fish_greeting ''

# base16 shell
eval sh $HOME/.config/base16-shell/base16-solarized.dark.sh

# ~/.config/fish/functions/add_to_path.fish
function add_to_path --description 'Persistently prepends paths to your PATH'
  set --universal fish_user_paths $fish_user_paths $argv
end

test -e {$HOME}/.iterm2_shell_integration.fish ; and source {$HOME}/.iterm2_shell_integration.fish
