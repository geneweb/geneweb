# $Id: Makefile,v 1.12 2000-08-18 15:15:25 ddr Exp $

include ../tools/Makefile.inc

OCAMLI=-I ../src
GWTP_OBJS=httpEnv.cmo iolight.cmo gwtp.cmo
GWTP_OTHER=../src/buff.cmo ../src/name.cmo ../src/iovalue.cmo ../src/adef.cmo ../src/gutil.cmo ../src/btree.cmo ../src/iobase.cmo

all:: out

out:: gwtp.out
	$(RM) gwtp
	cp gwtp.out gwtp

opt:: gwtp.opt
	$(RM) gwtp
	cp gwtp.opt gwtp
	$(STRIP) gwtp

clean::
	$(RM) gwtp

gwtp.out: $(GWTP_OBJS)
	$(OCAMLC) unix.cma $(GWTP_OTHER) $(GWTP_OBJS) -o gwtp.out

gwtp.opt: $(GWTP_OBJS:.cmo=.cmx)
	$(OCAMLOPT) unix.cmxa $(GWTP_OTHER:.cmo=.cmx) $(GWTP_OBJS:.cmo=.cmx) -o gwtp.opt

depend:
	../tools/camlp4_depend.sh $(OCAMLI) *.ml >> .depend.new
	mv .depend.new .depend

include .depend
