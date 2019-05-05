# Command line tools, terminals etc
{ pkgs, ... }:

with pkgs;

{
  home.packages = [
    alacritty
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
    (aspellWithDicts(dicts: with dicts; [ en nb ]))
  ];
}
