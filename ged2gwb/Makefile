# $Id: Makefile,v 1.1 1998-09-01 14:32:12 ddr Exp $

include ../tools/Makefile.inc

OCAMLI=-I ../src
OBJS=../src/argl.cmo ../src/adef.cmo ../src/iovalue.cmo ../src/name.cmo ../src/gutil.cmo ../src/btree.cmo ../src/iobase.cmo aurejac.cmo ansel.cmo ged2gwb.cmo

all:: ged2gwb.out
opt:: ged2gwb.opt

ged2gwb.out: $(OBJS)
	$(OCAMLC) -custom unix.cma $(LIBUNIX) $(CAMLP4D)/gramlib.cma $(OBJS) -o ged2gwb.out

ged2gwb.opt: $(OBJS:.cmo=.cmx)
	$(OCAMLOPT) unix.cmxa $(LIBUNIX) $(CAMLP4D)/gramlib.cmxa $(OBJS:.cmo=.cmx) -o ged2gwb.opt

depend:
	ocamldep $(OCAMLI) *.ml* > .depend

include .depend
