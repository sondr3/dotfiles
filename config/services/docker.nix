{ config, lib, ... }:

with lib;

let
  cfg = options.mine.docker;
in
{
  options.mine.docker.enable = mkEnableOption "docker";

  config = mkIf cfg.enable {
    virtualisation.docker = {
      enable = true;
      enableOnBoot = false;
    };
  };
}
