# $Id: Makefile,v 2.5 1999-09-14 22:33:34 ddr Exp $

include ../tools/Makefile.inc

OCAMLI=-I ../src
OBJS=../src/argl.cmo ../src/adef.cmo ../src/name.cmo ../src/ansel.cmo ../src/gutil.cmo ../src/iovalue.cmo ../src/btree.cmo ../src/buff.cmo ../src/iobase.cmo ../src/select.cmo ../src/version.cmo ../src/calendar.cmo gwb2ged.cmo

all:: gwb2ged.out
	$(RM) gwb2ged
	cp gwb2ged.out gwb2ged

opt:: gwb2ged.opt
	$(RM) gwb2ged
	cp gwb2ged.opt gwb2ged

clean::
	$(RM) gwb2ged

gwb2ged.out: $(OBJS)
	$(OCAMLC) -custom unix.cma $(CAMLP4D)/gramlib.cma $(OBJS) $(LIBUNIX) -o gwb2ged.out

gwb2ged.opt: $(OBJS:.cmo=.cmx)
	$(OCAMLOPT) unix.cmxa $(CAMLP4D)/gramlib.cmxa $(OBJS:.cmo=.cmx) $(LIBUNIX) -o gwb2ged.opt

depend:
	../tools/camlp4_depend.sh $(OCAMLI) *.ml* > .depend

include .depend
