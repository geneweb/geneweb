# $Id: Makefile,v 1.9 2000-08-17 16:41:14 ddr Exp $

CAMLP4=camlp4r
OCAMLC=ocamlc.opt
OCAMLOPT=ocamlopt.opt
OCOPT=-I ../src
RM=/bin/rm -f
GWTP_OBJS=httpEnv.cmo iolight.cmo gwtp.cmo
GWTP_OTHER=../src/buff.cmo ../src/name.cmo ../src/iovalue.cmo ../src/adef.cmo ../src/gutil.cmo ../src/btree.cmo ../src/iobase.cmo

all: gwtp.opt

gwtp.opt: $(GWTP_OBJS:.cmo=.cmx)
	$(OCAMLOPT) unix.cmxa $(GWTP_OTHER:.cmo=.cmx) $(GWTP_OBJS:.cmo=.cmx) -o gwtp.opt

gwtp.out: $(GWTP_OBJS)
	$(OCAMLC) unix.cma $(GWTP_OTHER) $(GWTP_OBJS) -o gwtp.out

clean:
	$(RM) *.cm[oix] *.o *.out *.opt

depend:
	> .depend.new
	for i in *.ml; do camlp4r pr_depend.cmo $$i >> .depend.new; done
	mv .depend.new .depend

.SUFFIXES: .ml .cmo .cmx

.ml.cmo:
	$(CAMLP4) pa_ru.cmo $< -o $*.ppo
	$(OCAMLC) $(OCOPT) -c -impl $*.ppo
	$(RM) $*.ppo

.ml.cmx:
	$(CAMLP4) pa_ru.cmo $< -o $*.ppo
	$(OCAMLOPT) $(OCOPT) -c -impl $*.ppo
	$(RM) $*.ppo

include .depend
