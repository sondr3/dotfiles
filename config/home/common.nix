{ ... }:

{
  nixpkgs = {
    overlays = [ (import ../pkgs/neovim.nix) ];
    config = {
      allowUnfree = true;
      sandbox = true;
    };
  };
}
