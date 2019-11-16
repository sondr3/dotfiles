{ config, lib, ... }:

with lib;

{
  options.mine.virtualbox.enable = mkEnableOption "virtualbox";

  config = mkIf config.mine.virtualbox.enable {
    virtualisation.virtualbox = {
      host.enable = true;
      host.enableExtensionPack = true;
      guest.enable = true;
    };
  };
}
