# $Id: Makefile,v 5.2 2010-09-23 17:16:49 ddr Exp $

include ../tools/Makefile.inc

OBJS=wserver.cmo

all:: pa_macro5.cmo $(OBJS)
opt:: pa_macro5.cmo $(OBJS:.cmo=.cmx)

pa_macro5.cmo: pa_macro5.ml
	camlp5r pa_extend.cmo q_MLast.cmo -o pa_macro5.ppo pa_macro5.ml
	ocamlc -c -I "`camlp5 -where`" -impl pa_macro5.ppo

depend:
	TOP=.. ../tools/camlp5_depend.sh $(OCAMLI) *.ml* > .depend

include .depend
