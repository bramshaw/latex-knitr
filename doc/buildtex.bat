Rscript -e "library(knitr);knit('./SPERA-main.Rnw')" 1> knitrOutput.log 2>&1

(@latex -synctex=1 "SPERA-main.tex" && bibtex "example" && latex "SPERA-main.tex" && latex "SPERA-main.tex" && dvips "SPERA-main.dvi" && ps2pdf "SPERA-main.ps") 1> latexOutput.log 2>&1