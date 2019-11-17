{ pkgs, config, lib, ... }:

with lib;

let
  cfg = options.mine.development.go;
in
{
  options.mine.development.go.enable = mkEnableOption "Go";

  # Enable sound with PulseAudio
  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      go
    ];
  };
}
