# $Id: Makefile,v 3.6 2000-06-13 12:43:27 ddr Exp $

include tools/Makefile.inc

all:: opt

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

distrib: new_distrib wrappers

wrappers:
	if test "$(CAMLP4F)" = "-DWIN95"; then \
	  echo 'cd gw' > distribution/gwd.bat; \
	  echo 'gwd' >> distribution/gwd.bat; \
	  echo 'cd gw' > distribution/gwsetup.bat; \
	  echo 'gwsetup' >> distribution/gwsetup.bat; \
	else \
	  echo '#!/bin/sh' > distribution/gwd; \
	  echo 'cd gw; exec ./gwd' >> distribution/gwd; \
	  echo '#!/bin/sh' > distribution/gwsetup; \
	  echo 'cd gw; exec ./gwsetup' >> distribution/gwsetup; \
	  chmod +x distribution/gwd distribution/gwsetup; \
	fi
	cp LICENSE distribution/LICENSE.txt

new_distrib: classical_distrib
	mkdir t
	mv distribution t/gw
	mv t distribution
	mkdir distribution/gw/old
	mkdir distribution/gw/setup
	cp setup/intro.txt distribution/gw/setup/.
	for i in de en es fr se; do \
	  mkdir distribution/gw/setup/$$i; \
	  cp setup/$$i/*.htm distribution/gw/setup/$$i; \
	  if test "$(CAMLP4F)" = "-DWIN95"; then \
	    cp setup/$$i/intro.txt.dos distribution/gw/setup/$$i/intro.txt; \
	  else \
	    cp setup/$$i/intro.txt distribution/gw/setup/$$i/intro.txt; \
	  fi; \
	done
	cp setup/gwsetup distribution/gw/gwsetup$(EXE)
	cp etc/README.distrib.txt distribution/README.txt
	cp etc/LISEZMOI.distrib.txt distribution/LISEZMOI.txt
	echo "127.0.0.1" > distribution/gw/only.txt

classical_distrib:
	$(RM) -rf distribution
	mkdir distribution
	cp CHANGES distribution/CHANGES.txt
	cp LICENSE distribution/LICENSE.txt
	cp src/gwc distribution/gwc$(EXE)
	cp src/consang distribution/consang$(EXE)
	cp src/gwd distribution/gwd$(EXE)
	cp src/gwu distribution/gwu$(EXE)
	cp ged2gwb/ged2gwb distribution/ged2gwb$(EXE)
	cp gwb2ged/gwb2ged distribution/gwb2ged$(EXE)
	cp etc/LISEZMOI.txt distribution/.
	cp etc/README.txt distribution/.
	cp etc/INSTALL.htm distribution/.
	cp etc/a.gwf distribution/.
	mkdir distribution/doc
	cp doc/*.htm distribution/doc/.
	for i in de fr en nl se; do \
	  mkdir distribution/doc/$$i; \
	  cp doc/$$i/*.htm distribution/doc/$$i/.; \
	done
	mkdir distribution/doc/images
	cp doc/images/*.jpg doc/images/gwlogo.gif distribution/doc/images/.
	mkdir distribution/lang
	cp hd/lang/*.txt distribution/lang/.
	for dir in af cn cs de dk en eo es fi fr he is it nl no pt ru se; do \
		mkdir distribution/lang/$$dir; \
		cp hd/lang/$$dir/start.txt distribution/lang/$$dir/.; \
	done
	mkdir distribution/images
	cp hd/images/*.jpg hd/images/*.gif distribution/images/.
	mkdir distribution/etc
	cp hd/etc/*.txt distribution/etc/.

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
