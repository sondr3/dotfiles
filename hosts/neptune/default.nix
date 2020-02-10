{ config, pkgs, ... }:

let hostname = "neptune";
in {
  imports = [ ./hardware.nix ../../system ];

  mine.gaming.enable = true;

  hardware = {
    cpu.amd.updateMicrocode = true;
    pulseaudio.extraConfig = ''
      # Required because it keeps switching to HDMI all the fucking time
      unload-module module-switch-on-port-available
    '';
  };

  networking.hostName = hostname;

  services.xserver.videoDrivers = [ "amdgpu" ];

  system.stateVersion = "19.03";
}
