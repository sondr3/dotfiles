{ config, lib, pkgs, ... }:

with lib;

let
  cfg = options.mine.gaming;
in
{
  options.mine.gaming.enable = mkEnableOption "Gaming";

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      minecraft
      steam
    ];
    # OpenGL support for Steam etc
    hardware = {
      opengl.driSupport32Bit = true;
      opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
      pulseaudio.support32Bit = true;
    };
  };
}
