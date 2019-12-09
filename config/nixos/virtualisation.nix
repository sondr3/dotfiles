{ lib, config, pkgs, ... }:

with lib;

{
  # Enable Docker
  virtualisation = {
    docker = {
      enable = true;
      enableOnBoot = false;
    };

    virtualbox = {
      host.enable = false;
      host.enableExtensionPack = false;
      guest.enable = false;
    };
  };

  environment.systemPackages = mkIf config.virtualisation.docker.enable [
    pkgs.docker-compose
  ];
}
