{ pkgs, lib, ... }:

with import ../../lib;

let sources = import ../../nix/sources.nix;
in {
  imports = [
    "${sources.home-manager}/nixos"
    ../modules
    ./audio.nix
    ./boot.nix
    ./cachix.nix
    ./common.nix
    ./fonts.nix
    ./gaming.nix
    ./programs.nix
    ./services.nix
    ./users.nix
    ./virtualisation.nix
  ];
}
