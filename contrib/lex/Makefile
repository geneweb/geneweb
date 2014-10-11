# $Id: Makefile.lex, v 0.1 2014-10-10 23:25:31 ipfix Exp $

EXEC = lex_utils

CAMLOPT = ocamlopt.opt
CAMLDEP = ocamldep

LIBS=str.cma

SOURCES = lex_utils.ml

all: depend $(EXEC).opt

OPTOBJS = $(SOURCES:.ml=.cmx)

$(EXEC).opt: $(OPTOBJS)
	$(CAMLOPT) $(LIBS:.cma=.cmxa) $(OPTOBJS) -o $(EXEC)

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmx:
	$(CAMLOPT) -c $<

clean:
	rm -f *.cm[iox] *~ .*~ *.o *.annot #*#
	rm -f $(EXEC)
	rm -f $(EXEC).opt

depend:
	$(CAMLDEP) *.mli *.ml > .depend

include .depend
