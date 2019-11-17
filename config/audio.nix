{ pkgs, lib, ... }:

with lib;

let
  cfg = options.mine.audio;
in
{
  options.mine.audio.enable = mkEnableOption "audio";

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
