{ pkgs, lib, ... }:

{
  programs.fish = {
    enable = true;
    shellInit = ''
      # TODO: Create better greeting
      set fish_greeting
      '' + lib.optionalString pkgs.stdenv.isDarwin ''
      if test -e '/Users/sondre/.nix-profile/etc/profile.d/nix.sh'
        bass source '/Users/sondre/.nix-profile/etc/profile.d/nix.sh'
        bass source '/Users/sondre/.config/fish/nix.sh'
      end
      '';
  };
  # funcs = builtins.attrNames (builtins.readDir ./functions);
  # funcFiles = map (file: xdg.configFile."fish/functions/${file}.text" = (file: builtins.toPath ./functions + "/${file}")) funcs;
}
