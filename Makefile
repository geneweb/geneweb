# $Id: Makefile,v 2.16 1999-08-04 01:37:48 ddr Exp $

include tools/Makefile.inc

all:: out

out::
	cd wserver; $(MAKE) all
	cd src; $(MAKE) all
	cd ged2gwb; $(MAKE) all
	cd gwb2ged; $(MAKE) all
	cd doc; $(MAKE) all
	cd setup; $(MAKE) all

opt::
	cd wserver; $(MAKE) opt
	cd src; $(MAKE) opt
	cd ged2gwb; $(MAKE) opt
	cd gwb2ged; $(MAKE) opt
	cd doc; $(MAKE) opt
	cd setup; $(MAKE) opt

distrib: classical_distrib
	mkdir t
	mv distribution t/gw
	mv t distribution
	cp setup/setup distribution/setup$(EXE)
	cp setup/gwd distribution/gwd$(EXE)
	mkdir distribution/gw/old
	mkdir distribution/gw/setup
	cp setup/intro.txt distribution/gw/setup/.
	for i in fr en es; do \
	  mkdir distribution/gw/setup/$$i; \
	  cp setup/$$i/*.htm distribution/gw/setup/$$i; \
	  cp setup/$$i/*.txt distribution/gw/setup/$$i; \
	done
	cp etc/ALIRE.distrib.txt distribution/README.txt
	cp etc/README.distrib.txt distribution/ALIRE.txt
	echo "127.0.0.1" > distribution/gw/only.txt

classical_distrib:
	$(RM) -rf distribution
	mkdir distribution
	cp CHANGES LICENSE distribution/.
	cp src/gwc distribution/gwc$(EXE)
	cp src/consang distribution/consang$(EXE)
	cp src/gwd distribution/gwd$(EXE)
	cp src/gwu distribution/gwu$(EXE)
	cp ged2gwb/ged2gwb distribution/ged2gwb$(EXE)
	cp gwb2ged/gwb2ged distribution/gwb2ged$(EXE)
	cp etc/ALIRE.txt distribution/.
	cp etc/README.txt distribution/.
	cp etc/INSTALL.htm distribution/.
	cp etc/a.gwf distribution/.
	cp etc/CREDITS.txt distribution/.
	mkdir distribution/doc
	cp doc/index.htm distribution/doc/.
	cp doc/LICENSE.htm distribution/doc/.
	for i in fr en nl se; do \
	  mkdir distribution/doc/$$i; \
	  cp doc/$$i/*.htm distribution/doc/$$i/.; \
	done
	mkdir distribution/lang
	cp hd/lang/*.txt distribution/lang/.
	for dir in cn cs de dk en eo es fr he it nl no pt se; do \
		mkdir distribution/lang/$$dir; \
		cp hd/lang/$$dir/start.txt distribution/lang/$$dir/.; \
	done
	mkdir distribution/images
	cp hd/images/*.gif distribution/images/.

clean::
	cd wserver; $(MAKE) clean
	cd src; $(MAKE) clean
	cd ged2gwb; $(MAKE) clean
	cd gwb2ged; $(MAKE) clean
	cd doc; $(MAKE) clean
	cd setup; $(MAKE) clean
	$(RM) -rf distribution
	$(RM) -f *~ .#*

depend:
	cd wserver; $(MAKE) depend
	cd src; $(MAKE) depend
	cd ged2gwb; $(MAKE) depend
	cd gwb2ged; $(MAKE) depend
	cd setup; $(MAKE) depend
