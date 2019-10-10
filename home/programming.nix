{ config, pkgs, ... }:

let
  # Link Yarn against latest Node instead of stable
  yarn = pkgs.yarn.override { nodejs = pkgs.nodejs-12_x; };
  pypacks = python-packages: with python-packages; [
    ipython
    requests
  ];
  python-with-packages = pkgs.python37.withPackages pypacks;
in
{
  home.packages = with pkgs; [
    # C, C++ and so on
    gcc9
    gnumake
    clang-tools

    # Go
    go

    # Node
    yarn
    nodejs-12_x

    # Haskell
    ghc
    hlint
    stack
    haskellPackages.apply-refact
    haskellPackages.brittany

    # Python
    python-with-packages

    # Rust
    # TODO Fix in nixpkgs
    # cargo-edit
    cargo-outdated
    rustup
  ];

  home.file.".npmrc".text = ''
    prefix = /home/sondre/.npm/
  '';
}
