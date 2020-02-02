{ pkgs, config, variables, lib, ... }:

with lib;

let cfg = config.mine.gaming;
in {
  options.mine.gaming.enable = mkEnableOption "Gaming";

  config = mkIf cfg.enable {
    # OpenGL support for Steam etc
    hardware = {
      opengl.driSupport32Bit = true;
      opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
      pulseaudio.support32Bit = true;
    };

    home-manager.users.sondre = {
      home.packages = with pkgs; [ minecraft steam ];
    };
  };
}
