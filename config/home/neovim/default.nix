{ config, pkgs, lib, ... }:

with import ../../../lib;

{
  programs.neovim = {
    enable = true;
    package = pkgs.neovim-unwrapped;
    viAlias = true;
    vimAlias = true;
    withNodeJs = false;
    withPython = false;
    withPython3 = true;
  };

  xdg.dataFile."nvim/site/autoload/plug.vim".source = pkgs.fetchFromGitHub {
    owner = "junegunn";
    repo = "vim-plug";
    rev = "68fef9c2fd9d4a21b500cc2249b6711a71c6fb9f";
    sha256 = "0azmnxq82frs375k5b9yjdvsjfmzjv92ifqnmniar19d96yh6swa";
  } + "/plug.vim";

  home.activation.neovim = execute ''
    ln -sfT /etc/nixos/config/home/neovim/init.vim ~/.config/nvim/init.vim
    vim +'PlugInstall --sync' +qa
  '';
}
