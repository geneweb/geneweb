# $Id: Makefile,v 1.1 2000-07-24 23:36:10 ddr Exp $

CAMLP4=camlp4r
OCAMLC=ocamlc
OCAMLCOPT=
RM=/bin/rm -f
GWTP_OBJS=httpEnv.cmo gwtp.cmo

all: gwtp.out

clean:
	$(RM) *.cm[oix] *.out

depend:
	> .depend.new
	for i in *.ml; do camlp4r pr_depend.cmo $$i >> .depend.new; done
	mv .depend.new .depend

gwtp.out: $(GWTP_OBJS)
	$(OCAMLC) $(GWTP_OBJS) -o gwtp.out

.SUFFIXES: .ml .cmo

.ml.cmo:
	$(CAMLP4) $< -o $*.ppo
	$(OCAMLC) $(OCAMLCOPT) -c -impl $*.ppo
	$(RM) $*.ppo

include .depend
