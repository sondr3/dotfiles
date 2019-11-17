{ config, lib, ... }:

with lib;

let
  cfg = options.mine.development;
in
{
  options.mine.development.enable = mkEnableOption "All development packages";

  config = mkIf cfg.enable {
    options.mine.development = {
      c.enable = true;
      direnv.enable = true;
      git.enable = true;
      go.enable = true;
      haskell.enable = true;
      latex.enable = true;
      node.enable = true;
      python.enable = true;
      rust.enable = true;
    };
  };
}
