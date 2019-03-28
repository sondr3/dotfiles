{ pkgs, ... }:

let
  vimPlugins = pkgs.vimPlugins;
in
{
  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;

    configure = {
      packages.myVimPackage = with vimPlugins; {
        start = [];
      };
    };
  };
}
