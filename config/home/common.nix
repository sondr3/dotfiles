{ ... }:

let sources = import ../../nix/sources.nix;
in {
  nixpkgs = {
    overlays = [ (import ../pkgs/default.nix) ];
    config = {
      allowUnfree = true;
      sandbox = true;
    };
  };
}
