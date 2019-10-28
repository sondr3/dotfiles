{ config, pkgs, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    ./neptune-hardware.nix
    ../system
    ../system/gaming.nix
  ];

  hardware = {
    cpu.amd.updateMicrocode = true;
    pulseaudio.extraConfig = ''
      # Required because it keeps switching to HDMI all the fucking time
      unload-module module-switch-on-port-available
    '';
  };

  networking.hostName = "neptune";

  services.xserver.videoDrivers = [ "amdgpu" ];

  home-manager.users.sondre = { pkgs, ... }: {
    imports = [
      # We can game on this machine
      ../home/gaming.nix
    ];
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?
}
