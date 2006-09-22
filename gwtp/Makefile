# $Id: Makefile,v 5.3 2006-09-22 19:26:59 ddr Exp $

include ../tools/Makefile.inc

OCAMLI=-I ../src
GWTP_OTHER=../src/lock.cmo ../src/buff.cmo ../src/name.cmo ../src/iovalue.cmo ../src/adef.cmo ../src/gwdb.cmo ../src/gutil.cmo ../src/btree.cmo ../src/secure.cmo ../src/iobase.cmo ../src/outbase.cmo ../src/translate.cmo
GWTP_OBJS=httpEnv.cmo iolight.cmo gwtp.cmo
OBJS=$(GWTP_OTHER) $(GWTP_OBJS)
RECOVER_OBJS=$(GWTP_OTHER) iolight.cmo recover.cmo

all:: out

out:: gwtp.out
	$(RM) gwtp
	cp gwtp.out gwtp

opt:: gwtp.opt
	$(RM) gwtp
	cp gwtp.opt gwtp
	$(STRIP) gwtp

recover: recover.out

clean::
	$(RM) gwtp

gwtp.out: $(GWTP_OBJS)
	$(OCAMLC) -custom $(STATIC) unix.cma $(OBJS) $(LIBUNIX) -o gwtp.out

gwtp.opt: $(GWTP_OBJS:.cmo=.cmx)
	$(OCAMLOPT) $(STATIC) unix.cmxa $(OBJS:.cmo=.cmx) $(LIBUNIX) -o gwtp.opt

recover.out: $(RECOVER_OBJS)
	$(OCAMLC) unix.cma $(RECOVER_OBJS) $(LIBUNIX) -o $@

depend:
	TOP=.. ../tools/camlp4_depend.sh $(OCAMLI) *.ml > .depend.new
	mv .depend.new .depend

include .depend
