# $Id: Makefile,v 1.1 2001-05-12 13:22:13 ddr Exp $

OCAMLC=ocamlc.opt
LIBUNIX=-cclib -lunix
GWDIFF_OBJS=../../src/argl.cmo ../../src/adef.cmo ../../src/name.cmo ../../src/buff.cmo ../../src/gutil.cmo ../../src/btree.cmo ../../src/iovalue.cmo ../../src/iobase.cmo ../../src/calendar.cmo gwdiff.cmo

all:: gwdiff.out

gwdiff.out: $(GWDIFF_OBJS)
	$(OCAMLC) -custom unix.cma $(GWDIFF_OBJS) $(LIBUNIX) -o gwdiff.out

clean::
	$(RM) *.out *.cm[oi]

gwdiff.cmo: gwdiff.ml
	camlp4r gwdiff.ml -o gwdiff.ppo
	ocamlc -I ../../src -c -impl gwdiff.ppo
	/bin/rm -f gwdiff.ppo
