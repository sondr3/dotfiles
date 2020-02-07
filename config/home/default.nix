{ pkgs, lib, config, ... }:

{
  imports = [
    ./common.nix
    ./desktop
    ./direnv.nix
    ./emacs
    ./fish
    ./git.nix
    ./gnupg.nix
    ./jetbrains.nix
    ./latex.nix
    ./mpv.nix
    ./neovim
    ./programming.nix
    ./social.nix
    ./terminal.nix
    ./vscode.nix
  ];
}
