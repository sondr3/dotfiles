{ pkgs, ... }:

{
  # Enable sound with PulseAudio
  hardware = {
    pulseaudio = {
      enable = true;
      package = pkgs.pulseaudioFull;
    };
  };
}
