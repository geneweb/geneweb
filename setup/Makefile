# $Id: Makefile,v 4.1 2001-05-14 12:38:02 ddr Exp $

include ../tools/Makefile.inc

OCAMLI=-I ../wserver -I ../src
OBJS=../wserver/wserver.cmo ../src/version.cmo ../src/argl.cmo setup.cmo

all:: out

out:: setup.out
	$(RM) gwsetup
	cp setup.out gwsetup

opt:: setup.opt
	$(RM) gwsetup
	cp setup.opt gwsetup
	$(STRIP) gwsetup

clean::
	$(RM) gwsetup *.bat
	$(RM) -rf test

test: test_dir wrappers

wrappers:
	if test "$(CAMLP4F)" = "-DWIN95"; then \
	  echo 'cd gw; gwd' > test/gwd.bat; \
	  echo 'cd gw; gwsetup' > test/gwsetup.bat; \
	else \
	  echo '#!/bin/sh' > test/gwd; \
	  echo 'cd gw; exec ./gwd' >> test/gwd; \
	  echo '#!/bin/sh' > test/gwsetup; \
	  echo 'cd gw; exec ./gwsetup' >> test/gwsetup; \
	  chmod +x test/gwd test/gwsetup; \
	fi

test_dir:
	mkdir test
	mkdir test/gw
	cd test/gw; ln -s ../../gwsetup gwsetup
	cd test/gw; ln -s ../.. setup
	cd test/gw; ln -s ../../../doc .
	cd test/gw; ln -s ../../../src/gwc .
	cd test/gw; ln -s ../../../src/gwd .
	cd test/gw; ln -s ../../../src/gwu .
	cd test/gw; ln -s ../../../src/consang .
	cd test/gw; ln -s ../../../hd/lang .
	cd test/gw; ln -s ../../../hd/images .
	cd test/gw; ln -s ../../../hd/etc .
	cd test/gw; ln -s ../../../ged2gwb/ged2gwb .
	cd test/gw; ln -s ../../../gwb2ged/gwb2ged .
	cd test/gw; ln -s ../../../etc/INSTALL.htm .
	cd test/gw; echo "127.0.0.1" > only.txt

setup.out: $(OBJS)
	$(OCAMLC) -custom unix.cma $(LIBUNIX) $(OBJS) -o setup.out

setup.opt: $(OBJS:.cmo=.cmx)
	$(OCAMLOPT) unix.cmxa $(LIBUNIX) $(OBJS:.cmo=.cmx) -o setup.opt

depend:
	TOP=.. ../tools/camlp4_depend.sh $(OCAMLI) *.ml* > .depend

always:

include .depend
