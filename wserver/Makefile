# $Id: Makefile,v 4.1 2001-05-14 12:38:04 ddr Exp $

include ../tools/Makefile.inc

OBJS=wserver.cmo

all:: $(OBJS)
opt:: $(OBJS:.cmo=.cmx)

depend:
	TOP=.. ../tools/camlp4_depend.sh $(OCAMLI) *.ml* > .depend

include .depend
