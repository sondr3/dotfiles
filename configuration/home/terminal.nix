# Command line tools, terminals etc
{ pkgs, ... }:

with pkgs;

{
  nixpkgs.overlays = [ (import ../../pkgs/default.nix) ];
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
