{ config, pkgs, ... }:

let
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
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
    # Go
    go

    # Node
    yarn
    nodejs-12_x

    # Haskell
    (all-hies.selection { selector = p: { inherit (p) ghc865; }; })
    cabal-install
    cabal2nix
    ghc
    ghcid
    haskellPackages.apply-refact
    haskellPackages.brittany
    haskellPackages.hoogle
    hlint
    stack
    stylish-cabal

    # Python
    python-with-packages

    # Rust
    # TODO Fix in nixpkgs
    # cargo-edit
    cargo-expand
    cargo-outdated
    rustup
  ];

  home.file.".npmrc".text = ''
    prefix = /home/sondre/.npm/
  '';
}
