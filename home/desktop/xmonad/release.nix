let pkgs = import <stable> { };
in pkgs.haskellPackages.callPackage ./default.nix { }
