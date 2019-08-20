{ config, pkgs, ... }:

# TODO: Remove all of the <unstable> once Brittany works on stable
let
  unstableTarball =
    fetchTarball
      https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz;
in
{

  nixpkgs.config = {
    packageOverrides = pkgs: {
      unstable = import unstableTarball {
        config = config.nixpkgs.config;
      };
    };
  };

  home.packages = with pkgs; [
    # Go
    go

    # Node
    nodejs-11_x

    # Haskell
    ghc
    hlint
    stack
    stack2nix
    unstable.haskellPackages.brittany

    # Python
    python37Packages.ipython

    # Rust
    cargo-edit
    cargo-outdated
    rustup

    # R
    R
  ];

  home.file.".npmrc".text = ''
    prefix = /home/sondre/.npm/
  '';
}
