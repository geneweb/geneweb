# $Id: Makefile,v 1.1.1.1 1998-09-01 14:32:14 ddr Exp $

include ../tools/Makefile.inc

OBJS=wserver.cmo

all:: $(OBJS)
opt:: $(OBJS:.cmo=.cmx)

depend:
	ocamldep $(OCAMLI) *.ml* > .depend

include .depend
