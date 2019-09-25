{ pkgs, lib, ... }:

# let
#   # Fully persisted and backed up links
#   mkPersistentLink = path: pkgs.runCommand "persistent-link" {} ''
#     ln -s ${path} $out
#   '';
# in
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

  # xdg.configFile."fish/functions/fish_prompt.fish".source = mkPersistentLink ./functions/fish_prompt.fish;
  # xdg.configFile."fish/functions/fish_right_prompt.fish".source = mkPersistentLink ./functions/fish_right_prompt.fish;
}
