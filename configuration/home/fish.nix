{ pkgs, ... }:

{
  programs.fish = {
    enable = true;
    shellInit = ''
      # TODO: Create better greeting
      set fish_greeting
      '' + (if pkgs.stdenv.isDarwin then ''
      if test -e '/Users/sondre/.nix-profile/etc/profile.d/nix.sh'
        bass source '/Users/sondre/.nix-profile/etc/profile.d/nix.sh'
        bass source '/Users/sondre/.config/fish/nix.sh'
      end
      '' else "");
  };
}
