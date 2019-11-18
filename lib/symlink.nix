{ lib, ... }:

with import <home-manager/modules/lib/dag.nix> { inherit lib; };

{
  symlink = src: dest: dagEntryAfter [ "installPackages" ] ''
    mkdir -p ${builtins.dirOf dest}
    ln -sf ${src} ${dest}
  '';
}
