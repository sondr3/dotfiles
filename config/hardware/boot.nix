{ pkgs, lib, ... }:

with lib;

{
  options.mine.boot.enable = mkEnableOption "boot?";

  # Use the systemd-boot EFI boot loader.
  config = mkIf config.mine.boot.enable {
    boot = {
      kernelPackages = pkgs.linuxPackages_latest;
      cleanTmpDir = true;
      plymouth.enable = true;
      loader.systemd-boot.enable = true;
      loader.efi.canTouchEfiVariables = true;
    };
  };
}
