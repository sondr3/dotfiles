{ pkgs, lib, ... }:

{
  imports = [
    ./home/common.nix
    ./home/desktop.nix
    ./home/emacs.nix
    ./home/fish
    ./home/git.nix
    ./home/jetbrains.nix
    ./home/latex.nix
    ./home/mpv.nix
    ./home/neovim.nix
    ./home/programming.nix
    ./home/vscode.nix
  ];
}
