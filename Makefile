#########################################
# Configuration part : where to install
#########################################

BINDIR = /users/demons/filliatr/bin/$(OSTYPE)

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

all: install

install: bibtex2html
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

MAJORVN=0
MINORVN=2
NAME=bibtex2html-$(MAJORVN).$(MINORVN)

FTP = /users/demons/filliatr/ftp/ocaml/bibtex2html
FILES = bibtex.mli latexmacros.ml Makefile bibtex_lexer.mll latexmacros.mli \
	translate.ml bbl_lexer.mll bibtex_parser.mly latexscan.mll \
	bibtex.ml html.ml main.ml .depend README COPYING GPL

export: $(FILES)
	mkdir -p export/bibtex2html
	cp $(FILES) export/bibtex2html
	(cd export ; tar cf $(NAME).tar bibtex2html ; \
	gzip -f --best $(NAME).tar)
	cp README COPYING GPL export/$(NAME).tar.gz $(FTP)

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

