{ pkgs, lib, ... }:

with lib;

let
  cfg = options.mine.fonts;
in
{
  options.mine.fonts.enable = mkEnableOption "fonts";

  config = mkIf cfg.enable {
    fonts = {
      enableFontDir = true;
      enableGhostscriptFonts = true;
      fontconfig.dpi = 221;
      fonts = with pkgs; [
        corefonts
        inconsolata
        fira
        fira-mono
        dejavu_fonts
        libertine
        ubuntu_font_family
        noto-fonts
        source-code-pro
      ];
    };
  };
}
