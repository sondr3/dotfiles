# Command line tools, terminals etc
{ pkgs, ... }:

with pkgs;

{
  home.packages = [
    alacritty
    (aspellWithDicts(ps: with ps; [ en nb ]))
    jq
    jump
    neofetch
    nix-prefetch-git
    nix-prefetch-github
    pandoc
    ripgrep
    tokei
    lm_sensors
    xorg.xkill
  ];
}
