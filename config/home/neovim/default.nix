{ config, pkgs, lib, ... }:

with import ../../../lib;

let
  sources = import ../../../nix/sources.nix;
  plug = sources.vim-plug;
in {
  programs.neovim = {
    enable = true;
    package = pkgs.neovim-unwrapped;
    viAlias = true;
    vimAlias = true;
    withNodeJs = true;
    withPython = false;
    withPython3 = true;
  };

  home.packages = with pkgs; [ mine.maple ];

  xdg.dataFile."nvim/site/autoload/plug.vim".source = pkgs.fetchFromGitHub {
    owner = plug.owner;
    repo = plug.repo;
    rev = plug.rev;
    sha256 = plug.sha256;
  } + "/plug.vim";

  home.activation.neovim = execute ''
    ln -sfT /etc/nixos/config/home/neovim/ ~/.config/nvim
    vim +'PlugInstall --sync' +qa
  '';
}
