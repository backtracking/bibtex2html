#########################################
# Configuration part : where to install
#########################################

BINDIR = $(HOME)/bin/$(OSTYPE)

#########################################
# End of configuration part
#########################################

MAJORVN=1
MINORVN=01

CAMLC    = ocamlc
CAMLCOPT = ocamlopt 
CAMLDEP  = ocamldep
ZLIBS    =
DEBUG    =
FLAGS    = $(ZLIBS) $(DEBUG)
PROFILE  =

STRLIB = -cclib -lstr 

OBJS =  latexmacros.cmx latexscan.cmx bbl_lexer.cmx \
	bibtex.cmx bibtex_lexer.cmx bibtex_parser.cmx \
	readbib.cmx expand.cmx bibfilter.cmx \
	html.cmx biboutput.cmx translate.cmx \
	version.cmx copying.cmx main.cmx

BIB2BIBOBJS = bibtex.cmx bibtex_lexer.cmx bibtex_parser.cmx readbib.cmx \
	condition.cmx \
	condition_parser.cmx condition_lexer.cmx parse_condition.cmx \
	bibfilter.cmx \
	html.cmx biboutput.cmx version.cmx copying.cmx bib2bib.cmx

all: bibtex2html bib2bib

install:
	cp bibtex2html bib2bib $(BINDIR)

bibtex2html: $(OBJS)
	ocamlopt $(PROFILE) $(FLAGS) -o bibtex2html str.cmxa $(OBJS) $(STRLIB)
	strip bibtex2html

bib2bib: $(BIB2BIBOBJS)
	ocamlopt $(PROFILE) $(FLAGS) -o bib2bib str.cmxa $(BIB2BIBOBJS) $(STRLIB)
	strip bib2bib

bibtex_parser.mli bibtex_parser.ml: bibtex_parser.mly
	ocamlyacc bibtex_parser.mly

condition_parser.mli condition_parser.ml: condition_parser.mly
	ocamlyacc condition_parser.mly

version.ml: Makefile
	echo "let version = \""$(MAJORVN).$(MINORVN)"\"" > version.ml
	echo "let date = \""`date`"\"" >> version.ml

latexscan.ml: latexscan.mll
	ocamllex latexscan.mll

bibtex_lexer.ml: bibtex_lexer.mll
	ocamllex bibtex_lexer.mll

condition_lexer.ml: condition_lexer.mll
	ocamllex condition_lexer.mll

bbl_lexer.ml: bbl_lexer.mll
	ocamllex bbl_lexer.mll

# export
########

NAME=bibtex2html-$(MAJORVN).$(MINORVN)

FTP = /users/demons/filliatr/ftp/ocaml/bibtex2html

FILES = bibtex.mli bibtex.ml latexmacros.mli latexmacros.ml bibtex_lexer.mll \
	translate.ml bbl_lexer.mll bibtex_parser.mly latexscan.mll \
	expand.mli expand.ml html.ml main.ml \
	Makefile .depend README COPYING GPL CHANGES \
	readbib.mli readbib.ml condition.mli condition.ml \
	condition_lexer.mli condition_lexer.mll condition_parser.mly \
	parse_condition.mli parse_condition.ml bibfilter.mli bibfilter.ml \
	biboutput.mli biboutput.ml bib2bib.ml copying.mli copying.ml \
	manual.tex

export: source doc linux solaris

move-olds:
	cp $(FTP)/bibtex2html* $(FTP)/olds

source: $(FILES)
	mkdir -p export/$(NAME)
	cp $(FILES) export/$(NAME)
	(cd export ; tar cf $(NAME).tar $(NAME) ; \
	gzip -f --best $(NAME).tar)
	cp README COPYING GPL CHANGES export/$(NAME).tar.gz $(FTP)

BINARY = bibtex2html-$(MAJORVN).$(MINORVN)-$(OSTYPE)

linux: clean binary
solaris:
	rmake sun-demons $(HOME)/soft/ocaml/bibtex clean binary
sunos4:
	rmake ??? $(HOME)/soft/ocaml/bibtex clean binary

binary: bibtex2html bib2bib
	mkdir -p export/$(BINARY)
	cp README COPYING GPL bibtex2html bib2bib export/$(BINARY)
	(cd export; tar czf $(BINARY).tar.gz $(BINARY))
	cp export/$(BINARY).tar.gz $(FTP)

AIX=bibtex2html-$(MAJORVN).$(MINORVN)-AIX

aix:
	mkdir -p export/$(AIX)
	cp README COPYING GPL bibtex2html bib2bib export/$(AIX)
	(cd export; tar cf $(AIX).tar $(AIX); gzip --best $(AIX).tar)

# documentation
###############

WWW=/users/demons/filliatr/WWW/bibtex2html

doc: manual.ps manual.html
	gzip -c manual.ps > $(WWW)/doc/manual.ps.gz
	cp -f manual.html $(WWW)/doc

manual.ps: manual.tex
	latex manual && latex manual
	dvips manual.dvi -o manual.ps

manual.html: manual.tex
	hevea manual.tex

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

