#########################################
# Configuration part : where to install
#########################################

BINDIR = $(HOME)/bin/$(OSTYPE)

#########################################
# End of configuration part
#########################################

MAJORVN=0
MINORVN=95

CAMLC    = ocamlc
CAMLCOPT = ocamlopt 
CAMLDEP  = ocamldep
ZLIBS    =
DEBUG    =
FLAGS    = $(ZLIBS) $(DEBUG)
PROFILE  =

STRLIB = -cclib -lstr 

OBJS = latexmacros.cmx latexscan.cmx bbl_lexer.cmx \
       bibtex.cmx bibtex_lexer.cmx bibtex_parser.cmx html.cmx \
       translate.cmx version.cmx main.cmx

all: bibtex2html

install:
	cp bibtex2html $(BINDIR)

bibtex2html: $(OBJS)
	ocamlopt $(PROFILE) $(FLAGS) -o bibtex2html str.cmxa $(OBJS) $(STRLIB)

bibtex_parser.mli bibtex_parser.ml: bibtex_parser.mly
	ocamlyacc bibtex_parser.mly

version.ml: Makefile
	echo "let version = \""$(MAJORVN).$(MINORVN)"\"" > version.ml
	echo "let date = \""`date`"\"" >> version.ml

latexscan.ml: latexscan.mll
	ocamllex latexscan.mll

bibtex_lexer.ml: bibtex_lexer.mll
	ocamllex bibtex_lexer.mll

bbl_lexer.ml: bbl_lexer.mll
	ocamllex bbl_lexer.mll

# export
########

NAME=bibtex2html-$(MAJORVN).$(MINORVN)

FTP = /users/demons/filliatr/ftp/ocaml/bibtex2html

FILES = bibtex.mli latexmacros.ml Makefile bibtex_lexer.mll latexmacros.mli \
	translate.ml bbl_lexer.mll bibtex_parser.mly latexscan.mll \
	bibtex.ml html.ml main.ml .depend README COPYING GPL CHANGES

export: source linux solaris

move-olds:
	cp $(FTP)/bibtex2html* $(FTP)/olds

source: $(FILES)
	mkdir -p export/bibtex2html
	cp $(FILES) export/bibtex2html
	(cd export ; tar cf $(NAME).tar bibtex2html ; \
	gzip -f --best $(NAME).tar)
	cp README COPYING GPL CHANGES export/$(NAME).tar.gz $(FTP)

BINARY = bibtex2html-$(MAJORVN).$(MINORVN)-$(OSTYPE)

linux: clean binary
solaris:
	rmake sun-demons $(HOME)/soft/ocaml/bibtex clean binary
sunos4:
	rmake ??? $(HOME)/soft/ocaml/bibtex clean binary

binary: bibtex2html
	mkdir -p export/$(BINARY)
	cp README COPYING GPL bibtex2html export/$(BINARY)
	(cd export; tar czf $(BINARY).tar.gz $(BINARY))
	cp export/$(BINARY).tar.gz $(FTP)

AIX=bibtex2html-$(MAJORVN).$(MINORVN)-AIX

aix:
	mkdir -p export/$(AIX)
	cp README COPYING GPL bibtex2html export/$(AIX)
	(cd export; tar cf $(AIX).tar $(AIX); gzip --best $(AIX).tar)

# generic rules :
#################

.SUFFIXES: .mli .ml .cmi .cmo .cmx
 
.mli.cmi:
	$(CAMLC) -c $(FLAGS) $<
 
.ml.cmo:
	$(CAMLC) -c $(FLAGS) $<

.ml.o:
	$(CAMLCOPT) -c $(FLAGS) $<

.ml.cmx:
	$(CAMLCOPT) -c $(PROFILE) $(FLAGS) $<


# clean and depend
##################

clean:
	rm -f *~ *.cm[iox] *.o bibtex_lexer.ml bibtex_parser.ml bibtex_parser.mli latexscan.ml bibtex2html bbl_lexer.ml

depend: bibtex_lexer.ml bbl_lexer.ml latexscan.ml \
	bibtex_parser.mli bibtex_parser.ml
	rm -f .depend
	ocamldep $(ZLIBS) *.mli *.ml > .depend

include .depend

