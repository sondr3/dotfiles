{ pkgs, config, lib, ... }:

with lib;

let
  unstable = import <unstable> {};
  cfg = options.mine.editors.neovim;
in
{
  options.mine.editors.neovim.enable = mkEnableOption "Neovim";

  config = mkIf cfg.enable {
    programs.neovim = {
      enable = true;
      package = unstable.neovim-unwrapped;
      viAlias = true;
      vimAlias = true;
      withNodeJs = true;
      withPython = false;
      withPython3 = true;

      extraConfig = ''
        filetype off

        call plug#begin(stdpath('data') . '/plugged')

        Plug 'sheerun/vim-polyglot'
        Plug 'neoclide/coc.nvim', {'branch': 'release'}
        Plug 'jacoborus/tender.vim'

        call plug#end()

        set termguicolors
        syntax enable
        colorscheme tender
      '';
    };

    xdg.dataFile."nvim/site/autoload/plug.vim".source = pkgs.fetchFromGitHub {
      owner = "junegunn";
      repo = "vim-plug";
      rev = "68fef9c2fd9d4a21b500cc2249b6711a71c6fb9f";
      sha256 = "0azmnxq82frs375k5b9yjdvsjfmzjv92ifqnmniar19d96yh6swa";
    } + "/plug.vim";

    xdg.configFile."nvim/coc-settings.json".text = (
      builtins.toJSON {
        languageserver = {
          haskell = {
            command = "hie-wrapper";
            filetypes = [ "hs" "lhs" "haskell" ];
            initializationOptions = {};
            rootPatterns = [ ".stack.yaml" "cabal.config" "package.yaml" ];
            settings = {
              languageServerHaskell = {
                completionSnippetsOn = true;
                hlintOn = true;
                maxNumberOfProblems = 25;
              };
            };
          };
        };
      }
    );
  };
}
