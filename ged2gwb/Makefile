# $Id: Makefile,v 2.2 1999-04-29 08:08:37 ddr Exp $

include ../tools/Makefile.inc

OCAMLI=-I ../src
OBJS=../src/argl.cmo ../src/adef.cmo ../src/iovalue.cmo ../src/name.cmo ../src/ansel.cmo ../src/gutil.cmo ../src/btree.cmo ../src/iobase.cmo aurejac.cmo ged2gwb.cmo

all:: ged2gwb.out
	$(RM) ged2gwb
	cp ged2gwb.out ged2gwb

opt:: ged2gwb.opt
	$(RM) ged2gwb
	cp ged2gwb.opt ged2gwb

clean::
	$(RM) ged2gwb

ged2gwb.out: $(OBJS)
	$(OCAMLC) -custom unix.cma $(LIBUNIX) $(CAMLP4D)/gramlib.cma $(OBJS) -o ged2gwb.out

ged2gwb.opt: $(OBJS:.cmo=.cmx)
	$(OCAMLOPT) unix.cmxa $(LIBUNIX) $(CAMLP4D)/gramlib.cmxa $(OBJS:.cmo=.cmx) -o ged2gwb.opt

depend:
	../tools/camlp4_depend.sh $(OCAMLI) *.ml* > .depend

include .depend
