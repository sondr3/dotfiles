{ ...}:

{
  # TODO: Add LaTeX packages
  home.file.".latexmkrc".text = ''
    # Use LuaLaTeX
    $lualatex='lualatex -interaction=nonstopmode -synctex=1 %O %S';
    $preview_continuous_mode = 1;
    # BibTeX with LuaLaTeX
    $bibtex_use = 2;
    # Create PDFs with LuaLaTeX
    $pdf_mode = 4;
    # TODO: Switch based on OS
    $pdf_previewer = 'open -a skim';
    # Remove SyncTeX generated stuff
    @generated_exts = (@generated_exts, 'synctex.gz');
  '';
}
