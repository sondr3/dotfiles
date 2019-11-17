{ pkgs, config, lib, ... }:

with lib;

let
  yarn = pkgs.yarn.override { nodejs = pkgs.nodejs-12_x; };
  cfg = options.mine.development.node;
in
{
  options.mine.development.node.enable = mkEnableOption "Node";

  # Enable sound with PulseAudio
  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      yarn
      nodejs-12_x
    ];

    home.file.".npmrc".text = ''
      prefix = /home/sondre/.npm/
    '';
  };
}
