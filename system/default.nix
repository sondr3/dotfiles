{ pkgs, lib, ... }:

{
  imports = [
    # Add home-manager module
    "${builtins.fetchGit {
      ref = "release-19.09";
      url = "https://github.com/rycee/home-manager";
    }}/nixos"
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
