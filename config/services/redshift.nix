{ lib, pkgs, ... }:

with lib;

let
  cfg = options.mine.redshift;
in
{
  options.mine.redshift.enable = mkEnableOption "Redshift";

  config = mkIf cfg.enable {

    location = {
      latitude = 63.39;
      longitude = 5.33;
    };
    services = {
      redshift.enable = true;
      redshift.temperature.day = 6500;
      redshift.temperature.night = 2300;
    };
  };
}
