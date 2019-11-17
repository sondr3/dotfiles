{ pkgs, config, lib, ... }:

with lib;

let
  pypacks = python-packages: with python-packages; [
    ipython
    requests
  ];
  python-with-packages = pkgs.python37.withPackages pypacks;
  cfg = options.mine.development.python;
in
{
  options.mine.development.python.enable = mkEnableOption "Python";

  # Enable sound with PulseAudio
  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      python-with-packages
    ];
  };
}
