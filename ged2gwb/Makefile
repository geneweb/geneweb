# $Id: Makefile,v 4.5 2004-08-09 11:34:59 ddr Exp $

include ../tools/Makefile.inc

OCAMLI=-I ../src
OBJS=../src/argl.cmo ../src/adef.cmo ../src/iovalue.cmo ../src/name.cmo ../src/ansel.cmo ../src/buff.cmo ../src/gutil.cmo ../src/btree.cmo ../src/secure.cmo ../src/iobase.cmo ../src/calendar.cmo ../src/lock.cmo ../src/consang.cmo ../src/consangAll.cmo ../src/check.cmo ged2gwb.cmo

all:: out

out:: ged2gwb.out
	$(RM) ged2gwb
	cp ged2gwb.out ged2gwb

opt:: ged2gwb.opt
	$(RM) ged2gwb
	cp ged2gwb.opt ged2gwb
	$(STRIP) ged2gwb

clean::
	$(RM) ged2gwb

ged2gwb.out: $(OBJS)
	$(OCAMLC) -custom $(STATIC) $(OCAMLD) unix.cma $(LIBUNIX) $(CAMLP4D)/gramlib.cma $(OBJS) -o ged2gwb.out

ged2gwb.opt: $(OBJS:.cmo=.cmx)
	$(OCAMLOPT) $(STATIC) unix.cmxa $(LIBUNIX) $(CAMLP4D)/gramlib.cmxa $(OBJS:.cmo=.cmx) -o ged2gwb.opt

depend:
	TOP=.. ../tools/camlp4_depend.sh $(OCAMLI) *.ml* > .depend

include .depend
