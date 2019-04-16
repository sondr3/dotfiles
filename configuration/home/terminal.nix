# Command line tools, terminals etc
{ pkgs, ... }:

{
  home.packages = with pkgs; [
    alacritty
    jq
    lm_sensors
    neofetch
    nix-prefetch-git
    nix-prefetch-github
    pandoc
    ripgrep
    tokei
    xorg.xkill
  ];
}
