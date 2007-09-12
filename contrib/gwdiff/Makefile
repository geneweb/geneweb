# $Id: Makefile,v 5.1 2007-09-12 09:58:44 ddr Exp $

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
	TOP=$(TOP) $(TOP)/tools/camlp5_depend.sh $(OCAMLI) `ls *.mli *.ml` > .depend.new
	mv .depend.new .depend

include .depend
