# $Id: Makefile,v 1.3 1998-09-29 12:22:31 ddr Exp $

include ../tools/Makefile.inc

OCAMLI=-I ../src
OBJS=../src/argl.cmo ../src/adef.cmo ../src/name.cmo ../src/gutil.cmo ../src/iovalue.cmo ../src/btree.cmo ../src/iobase.cmo ../wserver/wserver.cmo ../src/util.cmo ../src/select.cmo gwb2ged.cmo

all:: gwb2ged.out
	$(RM) gwb2ged
	cp gwb2ged.out gwb2ged

opt:: gwb2ged.opt
	$(RM) gwb2ged
	cp gwb2ged.opt gwb2ged

clean::
	$(RM) gwb2ged

gwb2ged.out: $(OBJS)
	ocamlc -custom unix.cma $(CAMLP4D)/gramlib.cma $(OBJS) $(LIBUNIX) -o gwb2ged.out

gwb2ged.opt: $(OBJS:.cmo=.cmx)
	ocamlopt unix.cmxa $(CAMLP4D)/gramlib.cmxa $(OBJS:.cmo=.cmx) $(LIBUNIX) -o gwb2ged.opt

depend:
	ocamldep $(OCAMLI) *.ml* > .depend

include .depend
