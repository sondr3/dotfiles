{ config, lib, ... }:

with lib;

let
  box = options.mine.virtualbox;
  dock = options.mine.docker;
in
{
  options.mine.virtualbox.enable = mkEnableOption "virtualbox";
  options.mine.docker.enable = mkEnableOption "docker";

  config = mkMerge [
    (
      mkIf box.enable {
        virtualisation.virtualbox = {
          host.enable = true;
          host.enableExtensionPack = true;
          guest.enable = true;
        };
      }
    )
    (
      mkIf dock.enable {
        virtualisation.docker = {
          enable = true;
          enableOnBoot = false;
        };
      }
    )
  ];
}
