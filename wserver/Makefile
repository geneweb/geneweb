# $Id: Makefile,v 4.0 2001-03-16 19:35:14 ddr Exp $

include ../tools/Makefile.inc

OBJS=wserver.cmo

all:: $(OBJS)
opt:: $(OBJS:.cmo=.cmx)

depend:
	../tools/camlp4_depend.sh $(OCAMLI) *.ml* > .depend

include .depend
