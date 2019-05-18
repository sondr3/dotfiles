{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  nixpkgs.overlays = [ (import ../pkgs/default.nix) ];

  imports = [
    ../configuration/home/common.nix
    ../configuration/home/emacs
    ../configuration/home/fish
    ../configuration/home/git.nix
    ../configuration/home/jetbrains.nix
    ../configuration/home/latex.nix
    ../configuration/home/mpv.nix
    ../configuration/home/neovim.nix
    ../configuration/home/terminal.nix
    ../configuration/home/vscode.nix
  ];
}
