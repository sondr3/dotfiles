{ pkgs, lib, ... }:

with import <home-manager/modules/lib/dag.nix> { inherit lib; };

{
  execute = cmd: dagEntryAfter [ "installPackages" ] ''
    ${cmd}
  '';
}
