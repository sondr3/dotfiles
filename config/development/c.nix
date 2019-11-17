{ pkgs, config, lib, ... }:

with lib;

let
  cfg = options.mine.development.c;
in
{
  options.mine.development.c.enable = mkEnableOption "C, C++";

  # Enable sound with PulseAudio
  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      gcc9
      gnumake
      clang-tools
    ];
  };
}
