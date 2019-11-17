{ lib, pkgs, ... }:

with lib;

let
  cfg = options.mine.commonServices;
in
{
  options.mine.commonServices.enable = mkEnableOption "Common services";

  config = mkIf cfg.enable {
    services = {
      # Enable the OpenSSH daemon.
      openssh.enable = true;
      # Enable CUPS to print documents.
      printing.enable = true;
      # Enable FSTrim for SSH health
      fstrim.enable = true;
      # Enable Dropbox
      dropbox.enable = true;
    };
  };
}
