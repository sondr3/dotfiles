{ pkgs, config, lib, ... }:

with lib;

let
  cfg = options.mine.editors;
in
{
  options.mine.editors.enable = mkEnableOption "All editors";

  config = mkIf cfg.enable {
    options.mine.editors = {
      emacs.enable = true;
      neovim.enable = true;
      jetbrains.enable = true;
      vscode.enable = true;
    };
  };
}
