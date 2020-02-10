{ pkgs, ... }:

let
  sources = import ../../nix/sources.nix;
  usenix = sources.nix-direnv;
in {
  programs.direnv = {
    enable = true;
    enableFishIntegration = true;
  };

  xdg.configFile."direnv/usenixrc".source = pkgs.fetchFromGitHub {
    owner = usenix.owner;
    repo = usenix.repo;
    rev = usenix.rev;
    sha256 = usenix.sha256;
  } + "/direnvrc";

  xdg.configFile."direnv/direnvrc".text = ''
    source $XDG_CONFIG_HOME/direnv/usenixrc
  '';
}
