
CAMLC    = ocamlc
CAMLCOPT = ocamlopt
CAMLDEP  = ocamldep
ZLIBS    =

OBJS = latexmacros.cmo latexscan.cmo \
       bibtex.cmo bibtex_lexer.cmo bibtex_parser.cmo html.cmo \
       translate.cmo main.cmo

all: bibtex2html

install: all
	cp bibtex2html /home/jcfillia/bin/`arch`

bibtex2html: $(OBJS)
	ocamlc -o bibtex2html $(OBJS)

bibtex_parser.mli bibtex_parser.ml: bibtex_parser.mly
	ocamlyacc bibtex_parser.mly

latexscan.ml: latexscan.mll
	ocamllex latexscan.mll

bibtex_lexer.ml: bibtex_lexer.mll
	ocamllex bibtex_lexer.mll


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
	rm -f *~ *.cm[iox] *.o bibtex_lexer.ml bibtex_parser.ml bibtex_parser.mli latexscan.ml bibtex2html

depend:
	rm -f .depend
	ocamldep $(ZLIBS) *.mli *.ml > .depend

include .depend

