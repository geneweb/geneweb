# $Id: Makefile,v 1.3 1998-09-24 12:57:18 ddr Exp $

include tools/Makefile.inc

all:: out

out::
	cd wserver; $(MAKE) all
	cd src; $(MAKE) all
	cd ged2gwb; $(MAKE) all
	cd gwb2ged; $(MAKE) all

opt::
	cd wserver; $(MAKE) opt
	cd src; $(MAKE) opt
	cd ged2gwb; $(MAKE) opt
	cd gwb2ged; $(MAKE) opt

distrib:
	$(RM) -rf distribution
	mkdir distribution
	cp CHANGES LICENCE distribution/.
	cp src/lgwc distribution/gwc$(EXE)
	cp src/lconsang distribution/consang$(EXE)
	cp src/lgwd distribution/gwd$(EXE)
	cp src/lgwu distribution/gwu$(EXE)
	cp ged2gwb/lged2gwb distribution/ged2gwb$(EXE)
	cp gwb2ged/lgwb2ged distribution/gwb2ged$(EXE)
	cp etc/ALIRE.txt distribution/.
	cp etc/README.txt distribution/.
	mkdir distribution/doc
	cp doc/index.htm distribution/doc/.
	mkdir distribution/doc/fr distribution/doc/en
	cp doc/fr/*.htm distribution/doc/fr/.
	cp doc/en/*.htm distribution/doc/en/.
	mkdir distribution/lang
	cp etc/a.cnf distribution/.
	cp hd/lang/*.txt distribution/lang/.
	for dir in de en eo es fr it nl se; do \
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
	$(RM) -rf distribution
	$(RM) -f *~ .#*

depend:
	cd wserver; $(MAKE) depend
	cd src; $(MAKE) depend
	cd ged2gwb; $(MAKE) depend
	cd gwb2ged; $(MAKE) depend
