{ pkgs, ... }:

let
  vimPlugins = pkgs.vimPlugins // pkgs.callPackage ./plugins.nix {};
in
{
  programs.neovim = {
    enable = if pkgs.stdenv.isDarwin then true else false;
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
