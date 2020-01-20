{ pkgs, variables, ... }:

{
  # Select internationalisation properties.
  i18n = {
    defaultLocale = "en_US.UTF-8";
  };
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Set your time zone.
  time.timeZone = "Europe/Oslo";

  nix = {
    autoOptimiseStore = true;
    nixPath = [
      "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixos"
      "nixos-config=/etc/nixos/hosts/${variables.hostname}/default.nix"
      "/nix/var/nix/profiles/per-user/root/channels"
    ];
  };

  nixpkgs = {
    config = {
      allowUnfree = true;
    };
  };

  location = {
    latitude = 63.39;
    longitude = 5.33;
  };

  networking = {
    networkmanager.enable = true;
    firewall.allowedTCPPorts = [
      8000 # Assorted web stuff
      3000 # React
    ];
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    acpi
    bind
    binutils
    cachix
    coreutils
    curl
    gitAndTools.gitFull
    htop
    niv
    neovim
    pciutils
    psmisc
    ranger
    tree
    unzip
    wget
    zip
  ];
}
