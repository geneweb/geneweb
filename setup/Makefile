# $Id: Makefile,v 1.5 1999-05-04 14:54:53 ddr Exp $

include ../tools/Makefile.inc

OCAMLI=-I ../wserver -I ../src
OBJS=../wserver/wserver.cmo ../src/version.cmo setup.cmo

all:: setup.out gwd.out
	$(RM) setup gwd
	cp setup.out setup
	cp gwd.out gwd

opt:: all

clean::
	$(RM) setup gwd
	$(RM) -rf test

test:
	mkdir test
	mkdir test/gw
	mkdir test/gw/old
	cd test; ln -s ../setup .
	cd test; ln -s ../gwd .
	cd test/gw; ln -s ../.. setup
	cd test/gw; ln -s ../../../doc .
	cd test/gw; ln -s ../../../src/gwc .
	cd test/gw; ln -s ../../../src/gwd .
	cd test/gw; ln -s ../../../src/gwu .
	cd test/gw; ln -s ../../../src/consang .
	cd test/gw; ln -s ../../../hd/lang .
	cd test/gw; ln -s ../../../hd/images .
	cd test/gw; ln -s ../../../ged2gwb/ged2gwb .
	cd test/gw; ln -s ../../../gwb2ged/gwb2ged .
	cd test/gw; ln -s ../../../etc/INSTALL.htm .
setup.out: $(OBJS)
	$(OCAMLC) -custom unix.cma $(LIBUNIX) $(OBJS) -o setup.out

gwd.out: gwd.cmo
	$(OCAMLC) -custom unix.cma $(LIBUNIX) gwd.cmo -o gwd.out

depend:
	../tools/camlp4_depend.sh $(OCAMLI) *.ml* > .depend

always:

include .depend
