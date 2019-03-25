{ pkgs, lib, ... }:

{
  imports = [
    ./home/common.nix
    ./home/fish.nix
    ./home/git.nix
    ./home/jetbrains.nix
    ./home/latex.nix
    ./home/mpv.nix
    ./home/vscode.nix
  ];
}
