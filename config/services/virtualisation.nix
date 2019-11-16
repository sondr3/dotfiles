{ config, lib, ... }:

with lib;

let
  cfg = options.mine.virtualbox;
in
{
  options.mine.virtualbox.enable = mkEnableOption "virtualbox";

  config = mkIf cfg.enable {
    virtualisation.virtualbox = {
      host.enable = true;
      host.enableExtensionPack = true;
      guest.enable = true;
    };
  };
}
