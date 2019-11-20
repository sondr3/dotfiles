{ config, pkgs, ... }:

let
  hostname = "uranus";
in
{
  imports =
    [
      ./hardware.nix
      ../../config/variables.nix
      ../../config/nixos
    ];

  variables.hostname = "uranus";
  variables.canGame = false;

  # Use the systemd-boot EFI boot loader.
  boot = {
    kernelModules = [ "acpi_call" ];
    extraModulePackages = with config.boot.kernelPackages; [ acpi_call ];
    kernelParams = [
      "mem_sleep_default=deep"
      "i915.fastboot=1"
      "i915.enable_dc=2"
      "i915.enable_fbc=1"
    ];
  };

  hardware = {
    cpu.intel.updateMicrocode = true;
    enableAllFirmware = true;

    # Bluetooth
    bluetooth = {
      enable = true;
      package = pkgs.bluezFull;
    };

    # OpenGL
    opengl = {
      extraPackages = with pkgs; [
        vaapiIntel
        vaapiVdpau
        libvdpau-va-gl
      ];
    };

    pulseaudio.extraModules = with pkgs; [
      pulseaudio-modules-bt
    ];
  };

  networking.hostName = "uranus";

  services = {
    # Enable TLP for power saving
    tlp.enable = true;
    # Enable firmware updating
    fwupd.enable = true;
    # Use libinput for touchpad
    xserver.libinput = {
      enable = true;
      disableWhileTyping = true;
      naturalScrolling = true;
      additionalOptions = ''
        Option "PalmDetection" "True"
      '';
    };
  };

  powerManagement = {
    cpuFreqGovernor = null;
    # Enable PowerTop auto-tuning
    powertop.enable = true;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?
}
