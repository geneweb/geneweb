# $Id: Makefile,v 1.10 2000-08-17 16:48:11 ddr Exp $

include ../tools/Makefile.inc

OCAMLI=-I ../src
GWTP_OBJS=httpEnv.cmo iolight.cmo gwtp.cmo
GWTP_OTHER=../src/buff.cmo ../src/name.cmo ../src/iovalue.cmo ../src/adef.cmo ../src/gutil.cmo ../src/btree.cmo ../src/iobase.cmo

all:: gwtp.opt

gwtp.opt: $(GWTP_OBJS:.cmo=.cmx)
	$(OCAMLOPT) unix.cmxa $(GWTP_OTHER:.cmo=.cmx) $(GWTP_OBJS:.cmo=.cmx) -o gwtp.opt

gwtp.out: $(GWTP_OBJS)
	$(OCAMLC) unix.cma $(GWTP_OTHER) $(GWTP_OBJS) -o gwtp.out

depend:
	> .depend.new
	for i in *.ml; do camlp4r pr_depend.cmo $$i >> .depend.new; done
	mv .depend.new .depend

include .depend
