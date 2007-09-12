# $Id: Makefile,v 5.20 2007-09-12 09:58:44 ddr Exp $

include ../tools/Makefile.inc

OCAMLI=-I ../src
OBJS=../src/argl.cmo ../src/adef.cmo ../src/iovalue.cmo ../src/buff.cmo ../src/name.cmo ../src/ansel.cmo ../src/mutil.cmo ../src/futil.cmo ../src/dutil.cmo ../src/secure.cmo ../src/btree.cmo ../src/database.cmo ../src/db2.cmo ../src/db2disk.cmo ../src/diff.cmo ../src/outbase.cmo ../src/gwdb.cmo ../src/checkItem.cmo ../src/calendar.cmo ../src/lock.cmo ../src/progrBar.cmo ../src/consang.cmo ../src/consangAll.cmo ../src/check.cmo ged2gwb.cmo

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
	$(OCAMLC) -custom $(STATIC) $(OCAMLD) unix.cma $(LIBUNIX) -I $(CAMLP5D) gramlib.cma $(OBJS) -o ged2gwb.out

ged2gwb.opt: $(OBJS:.cmo=.cmx)
	$(OCAMLOPT) $(STATIC) unix.cmxa $(LIBUNIX) -I $(CAMLP5D) gramlib.cmxa $(OBJS:.cmo=.cmx) -o ged2gwb.opt

depend:
	TOP=.. ../tools/camlp5_depend.sh $(OCAMLI) *.ml* > .depend

include .depend
