# $Id: Makefile,v 1.5 2000-07-26 01:39:33 ddr Exp $

CAMLP4=camlp4r
OCAMLC=ocamlc.opt
OCAMLOPT=ocamlopt.opt
OCOPT=-I ../geneweb/src
RM=/bin/rm -f
GWTP_OBJS=httpEnv.cmo gwtp.cmo
GWTP_OTHER=../geneweb/src/buff.cmo

all: gwtp.opt

gwtp.opt: $(GWTP_OBJS:.cmo=.cmx)
	$(OCAMLOPT) unix.cmxa $(GWTP_OTHER:.cmo=.cmx) $(GWTP_OBJS:.cmo=.cmx) -o gwtp.opt

gwtp.out: $(GWTP_OBJS)
	$(OCAMLC) $(GWTP_OTHER) $(GWTP_OBJS) -o gwtp.out

clean:
	$(RM) *.cm[oix] *.o *.out *.opt

depend:
	> .depend.new
	for i in *.ml; do camlp4r pr_depend.cmo $$i >> .depend.new; done
	mv .depend.new .depend

.SUFFIXES: .ml .cmo .cmx

.ml.cmo:
	$(CAMLP4) $< -o $*.ppo
	$(OCAMLC) $(OCOPT) -c -impl $*.ppo
	$(RM) $*.ppo

.ml.cmx:
	$(CAMLP4) $< -o $*.ppo
	$(OCAMLOPT) $(OCOPT) -c -impl $*.ppo
	$(RM) $*.ppo

include .depend
