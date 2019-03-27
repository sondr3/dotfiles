{ pkgs, stdenv, vimUtils, fetchurl, fetchFromGitHub }:

with vimUtils;

let
  yarn2nix = pkgs.callPackage (pkgs.fetchFromGitHub {
    owner = "moretea";
    repo = "yarn2nix";
    rev = "780e33a07fd821e09ab5b05223ddb4ca15ac663f";
    sha256 = "1f83cr9qgk95g3571ps644rvgfzv2i4i7532q8pg405s4q5ada3h";
  }) {};
in {
  coc-nvim = let
    pname = "coc.nvim";
    version = "0.0.62";

    src = fetchFromGitHub {
      owner = "neoclide";
      repo = pname;
      rev = "v${version}";
      sha256 = "1x0iivjyijrp69bl6j2ni74whnm2m30pcml0dv1b3311gdp4cy9r";
    };

    deps = yarn2nix.mkYarnModules rec {
      inherit version pname;
      name = "${pname}-modules-${version}";
      packageJSON = src + "/package.json";
      yarnLock = src + "/yarn.lock";
    };
  in buildVimPluginFrom2Nix {
    inherit version pname src;

    configurePhase = ''
      mkdir -p node_modules
      ln -s ${deps}/node_modules/* node_modules/
      ln -s ${deps}/node_modules/.bin node_modules/
    '';

    buildPhase = ''
      ${pkgs.yarn}/bin/yarn build
    '';

    postFixup = ''
      substituteInPlace $target/autoload/coc/util.vim \
        --replace "'yarnpkg'" "'${pkgs.yarn}/bin/yarnpkg'"
      substituteInPlace $target/autoload/health/coc.vim \
        --replace "'yarnpkg'" "'${pkgs.yarn}/bin/yarnpkg'"
    '';
  };
}
