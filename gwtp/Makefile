# $Id: Makefile,v 4.0 2001-03-16 19:32:15 ddr Exp $

include ../tools/Makefile.inc

OCAMLI=-I ../src
GWTP_OTHER=../src/lock.cmo ../src/buff.cmo ../src/name.cmo ../src/iovalue.cmo ../src/adef.cmo ../src/gutil.cmo ../src/btree.cmo ../src/iobase.cmo
GWTP_OBJS=httpEnv.cmo iolight.cmo gwtp.cmo
OBJS=$(GWTP_OTHER) $(GWTP_OBJS)

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
	$(OCAMLC) -custom unix.cma $(OBJS) $(LIBUNIX) -o gwtp.out

gwtp.opt: $(GWTP_OBJS:.cmo=.cmx)
	$(OCAMLOPT) unix.cmxa $(OBJS:.cmo=.cmx) $(LIBUNIX) -o gwtp.opt

depend:
	../tools/camlp4_depend.sh $(OCAMLI) *.ml > .depend.new
	mv .depend.new .depend

include .depend
