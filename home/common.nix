{ ... }:

let sources = import ../nix/sources.nix;
in {
  nixpkgs = {
    overlays = [ (import ../pkgs/default.nix) (import ../pkgs/awesome.nix) ];
    config = {
      allowUnfree = true;
      sandbox = true;
    };
  };
}
