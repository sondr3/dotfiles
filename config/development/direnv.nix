{ pkgs, config, lib, ... }:

with lib;

let
  cfg = options.direnv.enable;
in
{
  options.mine.direnv.enable = mkEnableOption "direnv";

  config = mkIf cfg.enable {
    programs.direnv = {
      enable = true;
      enableFishIntegration = true;
    };

    xdg.configFile."direnv/usenixrc".source = pkgs.fetchFromGitHub {
      owner = "nix-community";
      repo = "nix-direnv";
      rev = "f9889758694bdfe11251ac475d78a93804dbd93c";
      sha256 = "16mpc6lidmn6annyl4skdixzx7syvwdj9c5la0sidg57l8kh1rqd";
    } + "/direnvrc";

    xdg.configFile."direnv/direnvrc".text = ''
      source $XDG_CONFIG_HOME/direnv/usenixrc
    '';
  };
}
