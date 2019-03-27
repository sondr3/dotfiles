{ pkgs, ... }:

let
  vimPlugins = pkgs.vimPlugins // pkgs.callPackage ./plugins.nix {};
in
{
  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    withNodeJs = true;

    configure = {
      packages.myVimPackage = with vimPlugins; {
        start = [
          coc-nvim
	];
      };
    };
  };
}
