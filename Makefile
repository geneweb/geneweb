# $Id: Makefile,v 1.1 1998-09-01 14:32:01 ddr Exp $

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
	cp src/gwc.opt distribution/gwc$(EXE)
	cp src/consang.opt distribution/consang$(EXE)
	cp src/lgwd distribution/gwd$(EXE)
	cp src/gwu.opt distribution/gwu$(EXE)
	cp ged2gwb/ged2gwb.opt distribution/ged2gwb$(EXE)
	cp gwb2ged/gwb2ged.opt distribution/gwb2ged$(EXE)
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
	cp hd/images/up.gif distribution/images/.

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
