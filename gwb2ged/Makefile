# $Id: Makefile,v 1.2 1998-09-02 11:58:57 ddr Exp $

include ../tools/Makefile.inc

OCAMLI=-I ../src
OBJS=../src/argl.cmo ../src/adef.cmo ../src/name.cmo ../src/gutil.cmo ../src/iovalue.cmo ../src/btree.cmo ../src/iobase.cmo ../wserver/wserver.cmo ../src/util.cmo ../src/select.cmo gwb2ged.cmo

all:: gwb2ged.out
	$(RM) lgwb2ged
	cp gwb2ged.out lgwb2ged

opt:: gwb2ged.opt
	$(RM) lgwb2ged
	cp gwb2ged.opt lgwb2ged

clean::
	$(RM) lgwb2ged

gwb2ged.out: $(OBJS)
	ocamlc -custom unix.cma $(CAMLP4D)/gramlib.cma $(OBJS) $(LIBUNIX) -o gwb2ged.out

gwb2ged.opt: $(OBJS:.cmo=.cmx)
	ocamlopt unix.cmxa $(CAMLP4D)/gramlib.cmxa $(OBJS:.cmo=.cmx) $(LIBUNIX) -o gwb2ged.opt

depend:
	ocamldep $(OCAMLI) *.ml* > .depend

include .depend
