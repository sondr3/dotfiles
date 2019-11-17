{ pkgs, config, lib, ... }:

with lib;

let
  cfg = options.mine.latex;
in
{
  options.mine.latex.enable = mkEnableOption "LaTeX";

  config = mkIf cfg.enable {
    # TODO: Add LaTeX packages
    home.packages = with pkgs; [
      zathura
      texlive.combined.scheme-full
    ];

    home.file.".latexmkrc".text = ''
      # Use XeLaTeX
      $xelatex='xelatex -interaction=nonstopmode -synctex=1 %O %S';
      $preview_continuous_mode = 1;
      # BibTeX with XeLaTeX
      $bibtex_use = 2;
      # Create PDFs with XeLaTeX
      $pdf_mode = 5;
      # Remove SyncTeX generated stuff
      @generated_exts = (@generated_exts, 'synctex.gz');
      # Automatically open generated PDFs;
      $pdf_previewer = 'zathura';
    '';
  };
}
