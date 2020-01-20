{ ... }:

{
  nixpkgs = {
    overlays = [
      (import ../pkgs/default.nix)
      (import ../pkgs/neovim.nix)
    ];
    config = {
      allowUnfree = true;
      sandbox = true;
    };
  };
}
