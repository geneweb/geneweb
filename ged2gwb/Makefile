# $Id: Makefile,v 5.24 2008-01-15 17:12:01 ddr Exp $

include ../tools/Makefile.inc

OCAMLI=-I ../src
OBJS=../src/argl.cmo ../src/adef.cmo ../src/iovalue.cmo ../src/buff.cmo ../src/name.cmo ../src/ansel.cmo ../src/mutil.cmo ../src/futil.cmo ../src/dutil.cmo ../src/secure.cmo ../src/btree.cmo ../src/database.cmo ../src/db2.cmo ../src/db2disk.cmo ../src/diff.cmo ../src/outbase.cmo ../src/gwdb.cmo ../src/checkItem.cmo ../src/calendar.cmo ../src/lock.cmo ../src/progrBar.cmo ../src/consang.cmo ../src/consangAll.cmo ../src/check.cmo ged2gwb.cmo
OBJS2=../src/buff.cmo ../src/argl.cmo ../src/secure.cmo ../src/adef.cmo ../src/iovalue.cmo ../src/name.cmo ../src/mutil.cmo ../src/ansel.cmo ../src/calendar.cmo ../src/iochan.cmo ../src/db2.cmo ../src/progrBar.cmo ../src/futil.cmo ../src/notesLinks.cmo ../src/db2out.cmo ../src/db2disk.cmo ../src/dutil.cmo ../src/btree.cmo ../src/database.cmo ../src/gwdb.cmo ../src/consang.cmo ../src/consangAll.cmo ../src/checkItem.cmo ../src/check.cmo ../src/db2link.cmo ../src/lock.cmo ged2gwb2.cmo

all:: out

out:: ged2gwb.out ged2gwb2.out
	$(RM) ged2gwb ged2gwb2
	cp ged2gwb.out ged2gwb
	cp ged2gwb2.out ged2gwb2

opt:: ged2gwb.opt ged2gwb2.opt
	$(RM) ged2gwb ged2gwb2
	cp ged2gwb.opt ged2gwb
	cp ged2gwb2.opt ged2gwb2
	$(STRIP) ged2gwb ged2gwb2

clean::
	$(RM) ged2gwb ged2gwb2

ged2gwb.out: $(OBJS)
	$(OCAMLC) -custom $(STATIC) $(OCAMLD) unix.cma $(LIBUNIX) -I $(CAMLP5D) gramlib.cma $(OBJS) -o ged2gwb.out

ged2gwb.opt: $(OBJS:.cmo=.cmx)
	$(OCAMLOPT) $(STATIC) unix.cmxa $(LIBUNIX) -I $(CAMLP5D) gramlib.cmxa $(OBJS:.cmo=.cmx) -o ged2gwb.opt

ged2gwb2.out: $(OBJS2)
	$(OCAMLC) -custom $(STATIC) $(OCAMLD) unix.cma $(LIBUNIX) -I $(CAMLP5D) gramlib.cma $(OBJS2) -o ged2gwb2.out

ged2gwb2.opt: $(OBJS2:.cmo=.cmx)
	$(OCAMLOPT) $(STATIC) unix.cmxa $(LIBUNIX) -I $(CAMLP5D) gramlib.cmxa $(OBJS2:.cmo=.cmx) -o ged2gwb2.opt

depend:
	TOP=.. ../tools/camlp5_depend.sh $(OCAMLI) *.ml* > .depend

include .depend
