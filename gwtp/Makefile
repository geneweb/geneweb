# $Id: Makefile,v 1.7 2000-07-27 03:32:40 ddr Exp $

CAMLP4=camlp4r
OCAMLC=ocamlc.opt
OCAMLOPT=ocamlopt.opt
OCOPT=-I ../geneweb/src
RM=/bin/rm -f
GWTP_OBJS=httpEnv.cmo iolight.cmo gwtp.cmo
GWTP_OTHER=../geneweb/src/buff.cmo ../geneweb/src/name.cmo ../geneweb/src/iovalue.cmo ../geneweb/src/adef.cmo ../geneweb/src/gutil.cmo ../geneweb/src/btree.cmo ../geneweb/src/iobase.cmo
REBUILD_OBJS=iolight.cmo rebuild.cmo
REBUILD_OTHER=../geneweb/src/buff.cmo ../geneweb/src/name.cmo ../geneweb/src/iovalue.cmo ../geneweb/src/adef.cmo ../geneweb/src/gutil.cmo ../geneweb/src/btree.cmo ../geneweb/src/iobase.cmo ../geneweb/src/argl.cmo

all: gwtp.opt rebuild.opt

gwtp.opt: $(GWTP_OBJS:.cmo=.cmx)
	$(OCAMLOPT) unix.cmxa $(GWTP_OTHER:.cmo=.cmx) $(GWTP_OBJS:.cmo=.cmx) -o gwtp.opt

gwtp.out: $(GWTP_OBJS)
	$(OCAMLC) unix.cma $(GWTP_OTHER) $(GWTP_OBJS) -o gwtp.out

rebuild.opt: $(REBUILD_OBJS:.cmo=.cmx)
	$(OCAMLOPT) unix.cmxa $(REBUILD_OTHER:.cmo=.cmx) $(REBUILD_OBJS:.cmo=.cmx) -o rebuild.opt

rebuild.out: $(REBUILD_OBJS)
	$(OCAMLC) unix.cma $(REBUILD_OTHER) $(REBUILD_OBJS) -o rebuild.out

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
