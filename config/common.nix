{ pkgs, ... }:

{
  # Set your time zone.
  time.timeZone = "Europe/Oslo";
  networking = {
    networkmanager.enable = true;
    firewall.allowedTCPPorts = [
      8000 # Assorted web stuff
      3000 # React
    ];
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
}
