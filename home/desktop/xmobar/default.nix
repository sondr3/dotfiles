{ mkDerivation, base, containers, stdenv, X11, xmobar
, xmonad-contrib
}:
mkDerivation {
  pname = "myxmobar";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers X11 xmobar xmonad-contrib
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
