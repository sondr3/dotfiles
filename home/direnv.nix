{ pkgs, ... }:

{
  programs.direnv = {
    enable = true;
    enableFishIntegration = true;
  };

  xdg.configFile."direnv/direnvrc".source = ../scripts/direnvrc;
}
