{ pkgs, config, ... }:

let sources = import ../nix/sources.nix;
in {
  # Select internationalisation properties.
  i18n = { defaultLocale = "en_US.UTF-8"; };
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Set your time zone.
  time.timeZone = "Europe/Oslo";

  nix = {
    trustedUsers = [ "root" "sondre" "@wheel" ];
    autoOptimiseStore = true;
    nixPath = [
      "nixpkgs=${sources.unstable}"
      "stable=${sources.stable}"
      "nixos-config=/etc/nixos/hosts/${config.networking.hostName}/default.nix"
    ];
  };

  nixpkgs = {
    overlays = [ (self: super: rec { inherit sources; }) ];
    config = { allowUnfree = true; };
  };

  location.provider = "geoclue2";

  networking = {
    networkmanager.enable = true;
    firewall.allowedTCPPorts = [
      8000 # Assorted web stuff
      3000 # React
    ];
  };

  environment.variables = {
    GDK_SCALE = "1";
    GDK_DPI_SCALE = "0.5";
    QT_AUTO_SCREEN_SCALE_FACTOR = "1";
  };

  environment.systemPackages = with pkgs; [
    acpi
    bind
    binutils
    cachix
    coreutils
    curl
    gitAndTools.gitFull
    htop
    pciutils
    psmisc
    ranger
    unzip
    wget
    zip
  ];
}
