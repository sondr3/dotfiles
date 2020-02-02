{ pkgs, lib, ... }:

let sources = import ../nix/sources.nix;

in with import "${sources.home-manager}/modules/lib/dag.nix" { inherit lib; };

{
  execute = cmd:
    dagEntryAfter [ "installPackages" ] ''
      ${cmd}
    '';
}
