# $Id: Makefile,v 5.19 2007-09-12 09:58:44 ddr Exp $

include ../tools/Makefile.inc

OCAMLI=-I ../src
SRC_OBJS=../src/argl.cmo ../src/adef.cmo ../src/buff.cmo ../src/name.cmo ../src/ansel.cmo ../src/iovalue.cmo ../src/mutil.cmo ../src/futil.cmo ../src/dutil.cmo ../src/secure.cmo ../src/btree.cmo ../src/database.cmo ../src/db2.cmo ../src/db2disk.cmo ../src/diff.cmo ../src/gwdb.cmo ../src/checkItem.cmo ../src/gutil.cmo ../src/select.cmo ../src/version.cmo ../src/calendar.cmo
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
	$(OCAMLC) -custom $(STATIC) unix.cma -I $(CAMLP5D) gramlib.cma $(OBJS) $(LIBUNIX) -o gwb2ged.out

gwb2ged.opt: $(OBJS:.cmo=.cmx)
	$(OCAMLOPT) $(SRC_OBJS:.cmo=.cmx) -a -o x.cmxa
	$(OCAMLOPT) $(STATIC) unix.cmxa -I $(CAMLP5D) gramlib.cmxa x.cmxa gwb2ged.cmx $(LIBUNIX) -o gwb2ged.opt

depend:
	TOP=.. ../tools/camlp5_depend.sh $(OCAMLI) *.ml* > .depend

include .depend
