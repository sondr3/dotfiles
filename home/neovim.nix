{ pkgs, ... }:

let
  vimPlugins = pkgs.vimPlugins;
in
{
  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
  };
}
