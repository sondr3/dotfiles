# $pdflatex = 'pdflatex -interaction=nonstopmode -synctex=1 %O %S';
$lualatex='lualatex -interaction=nonstopmode -synctex=1 %O %S';
$preview_continuous_mode = 1;
$bibtex_use = 2;
$pdf_mode = 4;
$pdf_previewer = 'open -a skim';
# $clean_ext = 'bbl rel %R-blx.bib %R.synctex.gz';
@generated_exts = (@generated_exts, 'synctex.gz');
