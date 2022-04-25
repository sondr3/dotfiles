(local hm (require :heime))
(local utils (require :heime.utils))

(local template "
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
$pdf_previewer = 'zathura';
")

(hm.task {:name :latex
          :description "setup and configure latex"
          :run (fn [ctx data]
                 (ctx:write_string (ctx.user:home_file :.latexmkrc)
                                   (utils:template template {})))})
