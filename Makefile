#########################################
# Configuration part : where to install
#########################################

BINDIR = /home/jcfillia/bin/`arch`

#########################################
# End of configuration part
#########################################

CAMLC    = ocamlc
CAMLCOPT = ocamlopt
CAMLDEP  = ocamldep
ZLIBS    =

OBJS = latexmacros.cmo latexscan.cmo bbl_lexer.cmo \
       bibtex.cmo bibtex_lexer.cmo bibtex_parser.cmo html.cmo \
       translate.cmo main.cmo

all: bibtex2html

install: all
	cp bibtex2html $(BINDIR)

bibtex2html: $(OBJS)
	ocamlc -o bibtex2html $(OBJS)

bibtex_parser.mli bibtex_parser.ml: bibtex_parser.mly
	ocamlyacc bibtex_parser.mly

latexscan.ml: latexscan.mll
	ocamllex latexscan.mll

bibtex_lexer.ml: bibtex_lexer.mll
	ocamllex bibtex_lexer.mll

bbl_lexer.ml: bbl_lexer.mll
	ocamllex bbl_lexer.mll

# export
########

FTP = /home/jcfillia/ftp/ocaml/bibtex2html
FILES = bibtex.mli latexmacros.ml Makefile bibtex_lexer.mll latexmacros.mli \
	translate.ml bbl_lexer.mll bibtex_parser.mly latexscan.mll \
	bibtex.ml html.ml main.ml README COPYING GPL

export:
	mkdir -p BibTeX2HTML
	cp $(FILES) BibTeX2HTML
	tar cf bibtex2html.tar BibTeX2HTML
	gzip -f --best bibtex2html.tar
	cp README bibtex2html.tar.gz $(FTP)

# generic rules :
#################

.SUFFIXES: .mli .ml .cmi .cmo .cmx
 
.mli.cmi:
	$(CAMLC) -c $(ZLIBS) $<
 
.ml.cmo:
	$(CAMLC) -c $(ZLIBS) $<

.ml.o:
	$(CAMLCOPT) -c $(ZLIBS) $<

.ml.cmx:
	$(CAMLCOPT) -c $(ZLIBS) $<


# clean and depend
##################

clean:
	rm -f *~ *.cm[iox] *.o bibtex_lexer.ml bibtex_parser.ml bibtex_parser.mli latexscan.ml bibtex2html bbl_lexer.ml

depend:
	rm -f .depend
	ocamldep $(ZLIBS) *.mli *.ml > .depend

include .depend

