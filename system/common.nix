{ pkgs, ... }:

{
  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Europe/Oslo";

  nix = {
    autoOptimiseStore = true;
  };

  nixpkgs.config = {
    allowUnfree = true;
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
    gnupg
    htop
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
