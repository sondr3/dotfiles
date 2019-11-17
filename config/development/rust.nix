{ pkgs, config, lib, ... }:

with lib;

let
  cfg = config.mine.development.rust;
in
{
  options.mine.development.rust.enable = mkEnableOption "Rust";

  # Enable sound with PulseAudio
  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      # TODO Fix in nixpkgs
      # cargo-edit
      cargo-expand
      cargo-outdated
      rustup
    ];
  };
}
