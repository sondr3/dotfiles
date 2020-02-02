{ sources ? import ./sources.nix }:

with {
  overlay = _: pkgs: {
    niv = import sources.niv { };
    taffybar = import sources.taffybar { };
  };
};

import sources.nixpkgs {
  overlays = [ overlay ];
  config = { };
}
