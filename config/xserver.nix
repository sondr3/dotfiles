{ lib, pkgs, ... }:

with lib;

let
  cfg = options.mine.xorg;
in
{
  options.mine.xorg.enable = mkEnableOption "Graphical UI";

  config = mkIf cfg.enable {
    services = {
      # Enable the X11 windowing system.
      xserver = {
        enable = true;
        layout = "us,no";
        xkbOptions = "grp:alt_caps_toggle";
        exportConfiguration = true;

        # Enable the KDE Desktop Environment.
        displayManager.sddm.enable = true;
        desktopManager.plasma5.enable = true;
      };
    };
  };
}
