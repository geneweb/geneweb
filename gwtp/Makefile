# $Id: Makefile,v 1.2 2000-07-25 13:23:03 ddr Exp $

CAMLP4=camlp4r
OCAMLC=ocamlc
OCAMLCOPT=-I ../geneweb/src
RM=/bin/rm -f
GWTP_OBJS=httpEnv.cmo gwtp.cmo
GWTP_OTHER=../geneweb/src/buff.cmo

all: gwtp.out

gwtp.out: $(GWTP_OBJS)
	$(OCAMLC) $(GWTP_OTHER) $(GWTP_OBJS) -o gwtp.out

clean:
	$(RM) *.cm[oix] *.out

depend:
	> .depend.new
	for i in *.ml; do camlp4r pr_depend.cmo $$i >> .depend.new; done
	mv .depend.new .depend

.SUFFIXES: .ml .cmo

.ml.cmo:
	$(CAMLP4) $< -o $*.ppo
	$(OCAMLC) $(OCAMLCOPT) -c -impl $*.ppo
	$(RM) $*.ppo

include .depend
