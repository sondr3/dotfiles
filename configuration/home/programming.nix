{ pkgs, ... }:

let
  # Link Yarn against latest Node instead of stable
  yarn = pkgs.yarn.override { nodejs = pkgs.nodejs-11_x; };
in
{
  home.packages = with pkgs; [
    # Go
    go

    # Node
    yarn
    nodejs-11_x

    # Python
    python37Packages.ipython

    # Rust
    cargo-edit
    cargo-outdated
    rustup
  ];
}
