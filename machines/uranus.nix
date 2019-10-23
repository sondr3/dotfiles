{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./uranus-hardware.nix
      # Add home-manager module
      "${builtins.fetchGit {
        ref = "release-19.09";
        url = "https://github.com/rycee/home-manager";
      }}/nixos"
      ../system
    ];

  # Use the systemd-boot EFI boot loader.
  boot = {
    kernelModules = [ "acpi_call" ];
    extraModulePackages = with config.boot.kernelPackages; [ acpi_call ];
    kernelParams = [
      "mem_sleep_default=deep"
      "i915.enable_dc=2"
      "i915.enable_fbc=1"
      "i915.enable_psr=1"
      "i915.enable_guc=2"
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
        vaapiIntel vaapiVdpau libvdpau-va-gl
      ];
    };
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
