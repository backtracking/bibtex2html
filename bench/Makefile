
test:
	../bib2bib test.bib
	../bibtex2html -parse-only $$HOME/tex/bibliodemons/demons2.bib
	rm -f test.aux test.bbl
	latex test
	bibtex test
	latex test
	latex test
	xdvi test.dvi
