# Remove greeting
set -g -x fish_greeting ''

# Make Chruby work
source /usr/local/share/chruby/chruby.fish
source /usr/local/share/chruby/auto.fish
chruby 2.2.0

#direnv
eval (direnv hook fish)

# base16 shell
eval sh $HOME/.config/base16-shell/base16-solarized.dark.sh

# ~/.config/fish/functions/add_to_path.fish
function add_to_path --description 'Persistently prepends paths to your PATH'
  set --universal fish_user_paths $fish_user_paths $argv
end
