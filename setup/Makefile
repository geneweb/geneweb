# $Id: Makefile,v 1.2 1999-05-01 09:35:05 ddr Exp $

include ../tools/Makefile.inc

OCAMLI=-I ../wserver -I ../src
OBJS=../wserver/wserver.cmo ../src/version.cmo setup.cmo

all:: setup.out gw
	$(RM) setup
	cp setup.out setup

opt:: setup.opt gw
	$(RM) setup
	cp setup.opt setup

clean::
	$(RM) setup
	$(RM) -rf gw

gw:
	mkdir gw
	cd gw; ln -s ../../doc .
	cd gw; ln -s .. setup
	cd gw; ln -s ../../src/gwc .
	cd gw; ln -s ../../src/gwd .
	cd gw; ln -s ../../src/consang .
	cd gw; ln -s ../../hd/lang .
	cd gw; ln -s ../../hd/images .
	cd gw; ln -s ../../ged2gwb/ged2gwb .

setup.out: $(OBJS)
	$(OCAMLC) -custom unix.cma $(LIBUNIX) $(CAMLP4D)/gramlib.cma $(OBJS) -o setup.out

setup.opt: $(OBJS:.cmo=.cmx)
	$(OCAMLOPT) unix.cmxa $(LIBUNIX) $(CAMLP4D)/gramlib.cmxa $(OBJS:.cmo=.cmx) -o setup.opt

depend:
	../tools/camlp4_depend.sh $(OCAMLI) *.ml* > .depend

always:

include .depend
