# $Id: Makefile,v 1.2 2001-05-13 14:41:41 ddr Exp $

OCAMLC=ocamlc.opt
OCAMLOPT=ocamlopt.opt
LIBUNIX=-cclib -lunix
GWDIFF_OBJS=../../src/argl.cmo ../../src/adef.cmo ../../src/name.cmo ../../src/buff.cmo ../../src/gutil.cmo ../../src/btree.cmo ../../src/iovalue.cmo ../../src/iobase.cmo ../../src/calendar.cmo gwdiff.cmo

all:: gwdiff.opt

gwdiff.out: $(GWDIFF_OBJS)
	$(OCAMLC) -custom unix.cma $(GWDIFF_OBJS) $(LIBUNIX) -o gwdiff.out

gwdiff.opt: $(GWDIFF_OBJS:.cmo=.cmx)
	$(OCAMLOPT) unix.cmxa $(GWDIFF_OBJS:.cmo=.cmx) $(LIBUNIX) -o gwdiff.opt

clean::
	$(RM) *.cm[oix] *.o *.out *.opt

gwdiff.cmo: gwdiff.ml
	camlp4r gwdiff.ml -o gwdiff.ppo
	ocamlc -I ../../src -c -impl gwdiff.ppo
	/bin/rm -f gwdiff.ppo

gwdiff.cmx: gwdiff.ml
	camlp4r gwdiff.ml -o gwdiff.ppo
	ocamlopt -I ../../src -c -impl gwdiff.ppo
	/bin/rm -f gwdiff.ppo
