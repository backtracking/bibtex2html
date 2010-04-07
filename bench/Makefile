
test:
	-../bib2bib test.bib
	rm -f test.aux test.bbl
	latex test
	bibtex test
	latex test
	latex test
#	xdvi test.dvi

demons:
	../bibtex2html -parse-only $$HOME/tex/bibliodemons/demons2.bib
