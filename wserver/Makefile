# $Id: Makefile,v 1.2 1999-03-06 18:44:51 ddr Exp $

include ../tools/Makefile.inc

OBJS=wserver.cmo

all:: $(OBJS)
opt:: $(OBJS:.cmo=.cmx)

depend:
	../tools/camlp4_depend.sh $(OCAMLI) *.ml* > .depend

include .depend
