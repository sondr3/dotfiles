{ pkgs, lib, ... }:

{
  imports = [
    <home-manager/nixos>
    ../modules
    ./audio.nix
    ./boot.nix
    ./cachix.nix
    ./common.nix
    ./fonts.nix
    ./programs.nix
    ./services.nix
    ./users.nix
    ./virtualisation.nix
  ];
}
