{ pkgs, ... }:

let
  unstable = import <unstable> {};
in
{
  programs.neovim = {
    enable = true;
    package = unstable.neovim-unwrapped;
    viAlias = true;
    vimAlias = true;
    withNodeJs = true;
    withPython = false;
    withPython3 = true;

    extraConfig = ''
      call plug#begin(expand('~/.config/nvim/plugged'))

      Plug 'sheerun/vim-polyglot'
      Plug 'neoclide/coc.nvim', {'branch': 'release'}

      call plug#end()
    '';
  };

  xdg.dataFile."nvim/site/autoload/plug.vim".source = pkgs.fetchFromGitHub {
    owner = "junegunn";
    repo = "vim-plug";
    rev = "68fef9c2fd9d4a21b500cc2249b6711a71c6fb9f";
    sha256 = "0azmnxq82frs375k5b9yjdvsjfmzjv92ifqnmniar19d96yh6swa";
  } + "/plug.vim";
}
