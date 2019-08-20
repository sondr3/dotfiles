# Command line tools, terminals etc
{ pkgs, ... }:

with pkgs;

{
  home.packages = [
    alacritty
    (aspellWithDicts(ps: with ps; [ en nb ]))
    httpie
    jq
    jump
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