{ config, pkgs, ... }:

# TODO: Remove all of the <unstable> once Brittany works on stable
let
  unstableTarball =
    fetchTarball
      https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz;
  # Link Yarn against latest Node instead of stable
  yarn = pkgs.yarn.override { nodejs = pkgs.nodejs-11_x; };
  pypacks = python-packages: with python-packages; [
    ipython
    requests
  ];
  python-with-packages = pkgs.python37.withPackages pypacks;
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
    # C, C++ and so on
    gcc9
    gnumake
    clang-tools

    # Go
    go_1_12

    # Node
    yarn
    nodejs-11_x

    # Haskell
    ghc
    hlint
    stack
    stack2nix
    haskellPackages.apply-refact
    unstable.haskellPackages.brittany

    # Python
    python-with-packages

    # Rust
    cargo-edit
    cargo-outdated
    rustup
  ];

  home.file.".npmrc".text = ''
    prefix = /home/sondre/.npm/
  '';
}
