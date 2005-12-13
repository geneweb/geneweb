# $Id: Makefile,v 5.0 2005-12-13 11:51:27 ddr Exp $

include ../tools/Makefile.inc

OBJS=wserver.cmo

all:: $(OBJS)
opt:: $(OBJS:.cmo=.cmx)

depend:
	TOP=.. ../tools/camlp4_depend.sh $(OCAMLI) *.ml* > .depend

include .depend
