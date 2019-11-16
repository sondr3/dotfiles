{ config, lib, ... }:

with lib;

{
  options.mine.docker.enable = mkEnableOption "docker";

  config = mkIf config.mine.docker.enable {
    virtualisation.docker = {
      enable = true;
      enableOnBoot = false;
    };
  };
}
