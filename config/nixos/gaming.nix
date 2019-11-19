{ pkgs, config, variables, lib, ... }:

with lib;

{
  config = mkIf variables.canGame {
    # OpenGL support for Steam etc
    hardware = {
      opengl.driSupport32Bit = true;
      opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
      pulseaudio.support32Bit = true;
    };

    home-manager.users.sondre = {
      home.packages = with pkgs; [
        minecraft
        steam
      ];
    };
  };
}
