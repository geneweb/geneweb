# $Id: Makefile,v 3.4 2000-05-03 09:50:37 ddr Exp $

include ../tools/Makefile.inc

OCAMLI=-I ../src
OBJS=../src/argl.cmo ../src/adef.cmo ../src/iovalue.cmo ../src/name.cmo ../src/ansel.cmo ../src/buff.cmo ../src/gutil.cmo ../src/btree.cmo ../src/iobase.cmo ../src/calendar.cmo ged2gwb.cmo

all:: out

out:: ged2gwb.out
	$(RM) ged2gwb
	cp ged2gwb.out ged2gwb

opt:: ged2gwb.opt
	$(RM) ged2gwb
	cp ged2gwb.opt ged2gwb

clean::
	$(RM) ged2gwb

ged2gwb.out: $(OBJS)
	$(OCAMLC) -custom $(OCAMLD) unix.cma $(LIBUNIX) $(CAMLP4D)/gramlib.cma $(OBJS) -o ged2gwb.out

ged2gwb.opt: $(OBJS:.cmo=.cmx)
	$(OCAMLOPT) unix.cmxa $(LIBUNIX) $(CAMLP4D)/gramlib.cmxa $(OBJS:.cmo=.cmx) -o ged2gwb.opt

depend:
	../tools/camlp4_depend.sh $(OCAMLI) *.ml* > .depend

include .depend
