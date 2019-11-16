{ pkgs, lib, ... }:

{
  imports = [
    ./common.nix
    ./desktop.nix
    ./emacs
    ./direnv.nix
    ./fish
    ./git.nix
    ./jetbrains.nix
    ./latex.nix
    ./mpv.nix
    ./neovim.nix
    ./programming.nix
    ./social.nix
    ./terminal.nix
    ./vscode.nix
  ];
}
