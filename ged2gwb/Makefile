# $Id: Makefile,v 5.10 2006-10-04 14:17:54 ddr Exp $

include ../tools/Makefile.inc

OCAMLI=-I ../src
OBJS=../src/argl.cmo ../src/adef.cmo ../src/iovalue.cmo ../src/buff.cmo ../src/name.cmo ../src/ansel.cmo ../src/mutil.cmo ../src/dutil.cmo ../src/secure.cmo ../src/btree.cmo ../src/iobase.cmo ../src/gwdb.cmo ../src/checkItem.cmo ../src/gutil.cmo ../src/outbase.cmo ../src/calendar.cmo ../src/lock.cmo ../src/progrBar.cmo ../src/consang.cmo ../src/consangAll.cmo ../src/check.cmo ged2gwb.cmo

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
