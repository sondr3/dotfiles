{ pkgs, lib, ... }:

with lib;

let
  cfg = config.mine.audio;
in
{
  options.mine.audio.enable = lib.mkEnableOption "audio";

  # Enable sound with PulseAudio
  config = mkIf cfg.enable {
    hardware = {
      pulseaudio = {
        enable = true;
        package = pkgs.pulseaudioFull;
      };
    };
  };
}
