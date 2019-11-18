{ pkgs, lib, ... }:

with lib;
with import ../../../lib;

{
  programs.fish = {
    enable = true;
    shellInit = ''
      # TODO: Create better greeting
      set fish_greeting
    '';
    interactiveShellInit = ''
      source (jump shell fish | psub)
    '';
    shellAliases = {
      pbcopy = "xclip -selection clipboard";
      pbpaste = "xclip -o -selection clipboard";
    };
  };

  home.activation.fish = symlink "/etc/nixos/config/home/fish/functions/fish_prompt.fish" "~/.config/fish/functions/";
}
