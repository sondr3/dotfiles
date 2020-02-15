{ nixpkgs ? import <stable> { }, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, dbus, stdenv, taffybar, utf8-string, X11
    , xmonad, xmonad-contrib }:
    mkDerivation {
      pname = "myxmonad";
      version = "0.1.0.0";
      src = ./.;
      isLibrary = false;
      isExecutable = true;
      executableHaskellDepends =
        [ base containers dbus taffybar utf8-string X11 xmonad xmonad-contrib ];
      license = "unknown";
      hydraPlatforms = stdenv.lib.platforms.none;
    };

  haskellPackages = if compiler == "default" then
    pkgs.haskellPackages
  else
    pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f { });

in if pkgs.lib.inNixShell then drv.env else drv
