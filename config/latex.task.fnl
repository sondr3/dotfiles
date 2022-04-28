(let [hm (require :heime)
      utils (require :heime.utils)
      template "
# Use LuaLaTeX
$lualatex='lualatex --interaction=nonstopmode --synctex=1 --shell-escape %O %S';
$preview_continuous_mode = 1;
# BibTeX with LuaLaTeX
$bibtex_use = 2;
# Create PDFs with LuaLaTeX
$pdf_mode = 4;
# Remove SyncTeX generated stuff
@generated_exts = (@generated_exts, 'synctex.gz');
# Automatically open generated PDFs;
$pdf_previewer = 'zathura';"
      run (fn [ctx data]
            (ctx:write_string (ctx:home_file :.latexmkrc)
                              (utils:template template {})))]
  (hm.task {:name :latex :description "setup and configure latex" : run}))
