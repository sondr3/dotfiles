{ pkgs, lib, ... }:

{
  imports = [
    ./audio.nix
    ./boot.nix
    ./common.nix
    ./fonts.nix
    ./programs.nix
    ./services.nix
    ./users.nix
    ./virtualisation.nix
  ];
}
