# Makefile
# make build script for RGP's package vignette
#

all: pdf

rgp_introduction.tex:
	echo "Building package vignette LaTeX source..."
	"$(R_HOME)/bin/R" CMD sweave --encoding=utf8 rgp_introduction.Rnw 
	echo "DONE."

pdf: rgp_introduction.tex
	echo "Building package vignette PDF..."
	pdflatex rgp_introduction.tex
	bibtex rgp_introduction
	pdflatex rgp_introduction.tex
	pdflatex rgp_introduction.tex
	"$(R_HOME)/bin/Rscript" -e "tools::compactPDF('rgp_introduction.pdf', gs_quality='printer')"
	echo "DONE."

clean:
	echo "Cleaning up..."
	rm -f rgp_introduction.aux rgp_introduction.log rgp_introduction.out rgp_introduction.bbl rgp_introduction.blg rgp_introduction.tex texput.log
	echo "DONE."

distclean: clean
	echo "Removing all generated artifacts..."
	rm -f rgp_introduction.pdf
	rm -f rgp_introduction-*.pdf
	echo "DONE."

# EOF

