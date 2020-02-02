with import <nixpkgs> { };

with lib;

let
  getDir = (dir:
    mapAttrs
    (file: type: if type == "directory" then getDir "${dir}/${file}" else type)
    (builtins.readDir dir));

  dirFiles = dir:
    collect isString
    (mapAttrsRecursive (path: type: concatStringsSep "/" path) (getDir dir));

  recImport = dir:
    map (file: dir + "/${file}")
    (filter (file: (hasSuffix ".nix" file && file != "default.nix"))
      (dirFiles dir));

  utilPackages = map (m: callPackage (import m) { }) (recImport ./.);

in foldl' (x: y: x // y) { inherit getDir dirFiles recImport; } utilPackages
