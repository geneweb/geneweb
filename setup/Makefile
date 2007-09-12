# $Id: Makefile,v 5.1 2007-09-12 09:58:44 ddr Exp $

include ../tools/Makefile.inc

OCAMLI=-I ../wserver -I ../src
OBJS=../wserver/wserver.cmo ../src/version.cmo ../src/argl.cmo ../src/buff.cmo ../src/translate.cmo setup.cmo

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
	if test "$(CAMLP5F)" = "-DWIN95"; then \
	  echo 'cd gw' > test/gwd.bat; \
	  echo 'gwd' >> test/gwd.bat; \
	  echo 'cd gw' > test/gwsetup.bat; \
	  echo 'gwsetup' >> test/gwsetup.bat; \
	else \
	  (echo '#!/bin/sh'; \
	   echo 'mkdir -p bases'; \
	   echo 'cd bases'; \
	   echo 'exec ../gw/gwd -hd ../gw "$$@"') > test/gwd; \
	  (echo '#!/bin/sh'; \
	   echo 'mkdir -p bases'; \
	   echo 'cd bases'; \
	   echo 'exec ../gw/gwsetup -gd ../gw "$$@"') > test/gwsetup; \
	  chmod +x test/gwd test/gwsetup; \
	fi
	cd test; ln -s ../../etc/START.htm .

test_dir:
	mkdir test
	mkdir test/gw
	cd test/gw; ln -s ../../gwsetup gwsetup
	cd test/gw; ln -s ../.. setup
	cd test/gw; ln -s ../../../doc .
	cd test/gw; ln -s ../../../src/gwc .
	cd test/gw; cp ../../../src/gwd .
	cd test/gw; ln -s ../../../src/gwu .
	cd test/gw; ln -s ../../../src/consang .
	cd test/gw; ln -s ../../../hd/lang .
	cd test/gw; ln -s ../../../hd/images .
	cd test/gw; ln -s ../../../hd/etc .
	cd test/gw; ln -s ../../../ged2gwb/ged2gwb .
	cd test/gw; ln -s ../../../gwb2ged/gwb2ged .
	cd test/gw; ln -s ../../../etc/INSTALL.htm .
	cd test/gw; echo "127.0.0.1" > only.txt
	cd test/gw; echo "-setup_link" > gwd.arg

setup.out: $(OBJS)
	$(OCAMLC) -custom $(STATIC) unix.cma $(LIBUNIX) $(OBJS) -o setup.out

setup.opt: $(OBJS:.cmo=.cmx)
	$(OCAMLOPT) $(STATIC) unix.cmxa $(LIBUNIX) $(OBJS:.cmo=.cmx) -o setup.opt

depend:
	TOP=.. ../tools/camlp5_depend.sh $(OCAMLI) *.ml* > .depend

always:

include .depend
