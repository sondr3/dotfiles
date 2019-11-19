{ pkgs, lib, ... }:

with import ../../lib;

{
  imports = recImport ./.;
}
