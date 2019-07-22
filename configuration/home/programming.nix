{ pkgs, ... }:

{
  home.packages = with pkgs; [
    # Go
    go

    # Node
    nodejs-11_x

    # Python
    python37Packages.ipython

    # Rust
    cargo-edit
    cargo-outdated
    rustup
  ];

  home.file.".npmrc".text = ''
    prefix = /home/sondre/.npm/
  '';
}
