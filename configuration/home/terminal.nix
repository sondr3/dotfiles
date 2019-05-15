# Command line tools, terminals etc
{ pkgs, ... }:

with pkgs;

{
  home.packages = [
    alacritty
    (aspellWithDicts(dicts: with dicts; [ en nb ]))
    jq
    jump
    neofetch
    nix-prefetch-git
    nix-prefetch-github
    pandoc
    ripgrep
    tokei
  ] ++ (with pkgs; stdenv.lib.optionals stdenv.isLinux [
    lm_sensors
    xorg.xkill
  ]);
}
