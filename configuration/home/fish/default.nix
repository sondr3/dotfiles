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

      status is-interactive; and . (jump shell | psub)
      '' + lib.optionalString pkgs.stdenv.isDarwin ''
      if test -e '/Users/sondre/.nix-profile/etc/profile.d/nix.sh'
        bass source '/Users/sondre/.nix-profile/etc/profile.d/nix.sh'
        bass source '/Users/sondre/.config/fish/nix.sh'
      end
      '';
  };

  # xdg.configFile."fish/functions/fish_prompt.fish".source = mkPersistentLink ./functions/fish_prompt.fish;
  # xdg.configFile."fish/functions/fish_right_prompt.fish".source = mkPersistentLink ./functions/fish_right_prompt.fish;
}
