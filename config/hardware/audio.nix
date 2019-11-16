{ pkgs, lib, ... }:

{
  options.mine.audio.enable = lib.mkEnableOption "audio";

  # Enable sound with PulseAudio
  hardware = {
    pulseaudio = {
      enable = true;
      package = pkgs.pulseaudioFull;
    };
  };
}
