# $Id: Makefile,v 4.0 2001-03-16 19:32:10 ddr Exp $

include ../tools/Makefile.inc

OCAMLI=-I ../src
SRC_OBJS=../src/argl.cmo ../src/adef.cmo ../src/name.cmo ../src/ansel.cmo ../src/buff.cmo ../src/gutil.cmo ../src/iovalue.cmo ../src/btree.cmo ../src/iobase.cmo ../src/select.cmo ../src/version.cmo ../src/calendar.cmo
OBJS=$(SRC_OBJS) gwb2ged.cmo

all:: out

out:: gwb2ged.out
	$(RM) gwb2ged
	cp gwb2ged.out gwb2ged

opt:: gwb2ged.opt
	$(RM) gwb2ged
	cp gwb2ged.opt gwb2ged
	$(STRIP) gwb2ged

clean::
	$(RM) gwb2ged

gwb2ged.out: $(OBJS)
	$(OCAMLC) -custom unix.cma $(CAMLP4D)/gramlib.cma $(OBJS) $(LIBUNIX) -o gwb2ged.out

gwb2ged.opt: $(OBJS:.cmo=.cmx)
	$(OCAMLOPT) $(SRC_OBJS:.cmo=.cmx) -a -o x.cmxa
	$(OCAMLOPT) unix.cmxa $(CAMLP4D)/gramlib.cmxa x.cmxa gwb2ged.cmx $(LIBUNIX) -o gwb2ged.opt

depend:
	../tools/camlp4_depend.sh $(OCAMLI) *.ml* > .depend

include .depend
