# $Id: Makefile,v 2.5 1999-04-30 11:45:46 ddr Exp $

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

setup_distrib: distrib
	mkdir t
	mv distribution t/gw
	mv t distribution
	cp setup/setup distribution/.
	mkdir distribution/gw/setup
	cp setup/intro.txt distribution/gw/setup/.
	for i in fr; do \
	  mkdir distribution/gw/setup/$$i; \
	  cp setup/$$i/* distribution/gw/setup/$$i; \
	done

distrib:
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
	for dir in cn de dk en eo es fr he it nl no pt se; do \
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
	$(RM) -rf distribution
	$(RM) -f *~ .#*

depend:
	cd wserver; $(MAKE) depend
	cd src; $(MAKE) depend
	cd ged2gwb; $(MAKE) depend
	cd gwb2ged; $(MAKE) depend
