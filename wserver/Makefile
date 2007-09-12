# $Id: Makefile,v 5.1 2007-09-12 09:58:44 ddr Exp $

include ../tools/Makefile.inc

OBJS=wserver.cmo

all:: $(OBJS)
opt:: $(OBJS:.cmo=.cmx)

depend:
	TOP=.. ../tools/camlp5_depend.sh $(OCAMLI) *.ml* > .depend

include .depend
