# $Id: Makefile,v 1.3 2001-05-14 12:38:01 ddr Exp $

include ../../tools/Makefile.inc
TOP=../..

OCAMLI=-I $(TOP)/src
GWDIFF_OBJS=$(TOP)/src/argl.cmo $(TOP)/src/adef.cmo $(TOP)/src/name.cmo $(TOP)/src/buff.cmo $(TOP)/src/gutil.cmo $(TOP)/src/btree.cmo $(TOP)/src/iovalue.cmo $(TOP)/src/iobase.cmo $(TOP)/src/calendar.cmo gwdiff.cmo

all:: gwdiff.opt

gwdiff.out: $(GWDIFF_OBJS)
	$(OCAMLC) -custom unix.cma $(GWDIFF_OBJS) $(LIBUNIX) -o gwdiff.out

gwdiff.opt: $(GWDIFF_OBJS:.cmo=.cmx)
	$(OCAMLOPT) unix.cmxa $(GWDIFF_OBJS:.cmo=.cmx) $(LIBUNIX) -o gwdiff.opt

depend:
	TOP=$(TOP) $(TOP)/tools/camlp4_depend.sh $(OCAMLI) `ls *.mli *.ml` > .depend.new
	mv .depend.new .depend

include .depend
