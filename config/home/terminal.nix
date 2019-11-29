# Command line tools, terminals etc
{ pkgs, ... }:

with pkgs;

{
  home.packages = [
    (aspellWithDicts (ps: with ps; [ en nb ]))
    alacritty
    fd
    httpie
    jq
    jump
    lm_sensors
    neofetch
    nix-prefetch-git
    nix-prefetch-github
    nixpkgs-fmt
    pandoc
    ripgrep
    tokei
    xclip
    xorg.xkill
    xsv
  ];
}
