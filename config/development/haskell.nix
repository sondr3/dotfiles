{ pkgs, config, lib, ... }:

with lib;

let
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
  cfg = config.mine.development.haskell;
in
{
  options.mine.development.haskell.enable = mkEnableOption "Haskell";

  # Enable sound with PulseAudio
  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      (all-hies.selection { selector = p: { inherit (p) ghc865; }; })
      cabal-install
      cabal2nix
      ghc
      ghcid
      haskellPackages.apply-refact
      haskellPackages.brittany
      haskellPackages.hoogle
      hlint
      stack
    ];
  };
}
