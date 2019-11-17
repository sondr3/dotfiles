{ pkgs, lib, ... }:

with lib;

let
  cfg = config.mine.boot;
in
{
  options.mine.boot.enable = mkEnableOption "Boot with systemd-boot";

  # Use the systemd-boot EFI boot loader.
  config = mkIf cfg.enable {
    boot = {
      kernelPackages = pkgs.linuxPackages_latest;
      cleanTmpDir = true;
      plymouth.enable = true;
      loader.systemd-boot.enable = true;
      loader.efi.canTouchEfiVariables = true;
    };
  };
}
