{ pkgs, lib, ... }:

with import ../lib;

{
  imports = recImport ./. ++ [
    "${builtins.fetchGit {
      ref = "release-19.09";
      url = "https://github.com/rycee/home-manager";
    }}/nixos"
  ];
}
