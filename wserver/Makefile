# $Id: Makefile,v 3.0 1999-10-29 10:31:50 ddr Exp $

include ../tools/Makefile.inc

OBJS=wserver.cmo

all:: $(OBJS)
opt:: $(OBJS:.cmo=.cmx)

depend:
	../tools/camlp4_depend.sh $(OCAMLI) *.ml* > .depend

include .depend
