{ pkgs, lib, ... }:

with import ../../lib;

{
  imports = [ <home-manager/nixos> ../modules ] ++ recImport ./.;
}
