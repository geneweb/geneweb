# $Id: Makefile,v 2.1 1999-03-08 11:19:42 ddr Exp $

include ../tools/Makefile.inc

OBJS=wserver.cmo

all:: $(OBJS)
opt:: $(OBJS:.cmo=.cmx)

depend:
	../tools/camlp4_depend.sh $(OCAMLI) *.ml* > .depend

include .depend
