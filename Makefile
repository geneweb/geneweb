# $Id: Makefile,v 3.19 2001-02-20 23:00:36 ddr Exp $

DESTDIR=distribution

include tools/Makefile.inc

all:: opt

out::
	cd wserver; $(MAKE) all
	cd src; $(MAKE) all
	cd ged2gwb; $(MAKE) all
	cd gwb2ged; $(MAKE) all
	cd doc; $(MAKE) all
	cd setup; $(MAKE) all
	cd gwtp; $(MAKE) all

opt::
	cd wserver; $(MAKE) opt
	cd src; $(MAKE) opt
	cd ged2gwb; $(MAKE) opt
	cd gwb2ged; $(MAKE) opt
	cd doc; $(MAKE) opt
	cd setup; $(MAKE) opt
	cd gwtp; $(MAKE) opt

distrib: new_distrib wrappers

wrappers:
	if test "$(CAMLP4F)" = "-DWIN95"; then \
	  echo 'cd gw' > $(DESTDIR)/gwd.bat; \
	  echo 'gwd' >> $(DESTDIR)/gwd.bat; \
	  echo 'cd gw' > $(DESTDIR)/gwsetup.bat; \
	  echo 'gwsetup' >> $(DESTDIR)/gwsetup.bat; \
	else \
	  echo '#!/bin/sh' > $(DESTDIR)/gwd; \
	  echo 'cd gw; exec ./gwd' >> $(DESTDIR)/gwd; \
	  echo '#!/bin/sh' > $(DESTDIR)/gwsetup; \
	  echo 'cd gw; exec ./gwsetup' >> $(DESTDIR)/gwsetup; \
	  chmod +x $(DESTDIR)/gwd $(DESTDIR)/gwsetup; \
	fi

new_distrib: classical_distrib
	mkdir t
	mv $(DESTDIR) t/gw
	mv t $(DESTDIR)
	mkdir $(DESTDIR)/gw/old
	mkdir $(DESTDIR)/gw/setup
	cp setup/intro.txt $(DESTDIR)/gw/setup/.
	for i in de en es fr lv se; do \
	  mkdir $(DESTDIR)/gw/setup/$$i; \
	  cp setup/$$i/*.htm $(DESTDIR)/gw/setup/$$i; \
	  if test "$(CAMLP4F)" = "-DWIN95"; then \
	    cp setup/$$i/intro.txt.dos $(DESTDIR)/gw/setup/$$i/intro.txt; \
	  else \
	    cp setup/$$i/intro.txt $(DESTDIR)/gw/setup/$$i/intro.txt; \
	  fi; \
	done
	cp setup/gwsetup $(DESTDIR)/gw/gwsetup$(EXE)
	for i in README LISEZMOI; do \
	  echo "<pre>" > $(DESTDIR)/$$i.htm; \
	  cat etc/$$i.distrib.txt >> $(DESTDIR)/$$i.htm; \
	  echo "</pre>" >> $(DESTDIR)/$$i.htm; \
	done
	cp LICENSE $(DESTDIR)/LICENSE.txt
	cp etc/START.htm $(DESTDIR)/.
	echo "127.0.0.1" > $(DESTDIR)/gw/only.txt

classical_distrib:
	$(RM) -rf $(DESTDIR)
	mkdir $(DESTDIR)
	cp CHANGES $(DESTDIR)/CHANGES.txt
	cp LICENSE $(DESTDIR)/LICENSE.txt
	cp src/gwc $(DESTDIR)/gwc$(EXE)
	cp src/consang $(DESTDIR)/consang$(EXE)
	cp src/gwd $(DESTDIR)/gwd$(EXE)
	cp src/gwu $(DESTDIR)/gwu$(EXE)
	cp ged2gwb/ged2gwb $(DESTDIR)/ged2gwb$(EXE)
	cp gwb2ged/gwb2ged $(DESTDIR)/gwb2ged$(EXE)
	mkdir $(DESTDIR)/gwtp_tmp
	mkdir $(DESTDIR)/gwtp_tmp/lang
	cp gwtp/gwtp $(DESTDIR)/gwtp_tmp/gwtp$(EXE)
	cp gwtp/README $(DESTDIR)/gwtp_tmp/.
	for i in en fr; do \
		mkdir $(DESTDIR)/gwtp_tmp/lang/$$i; \
		cp gwtp/lang/$$i/*.txt $(DESTDIR)/gwtp_tmp/lang/$$i/.; \
	done
	cp etc/LISEZMOI.txt $(DESTDIR)/.
	cp etc/README.txt $(DESTDIR)/.
	cp etc/INSTALL.htm $(DESTDIR)/.
	cp etc/a.gwf $(DESTDIR)/.
	mkdir $(DESTDIR)/doc
	cp doc/*.htm $(DESTDIR)/doc/.
	for i in de fr en nl se; do \
	  mkdir $(DESTDIR)/doc/$$i; \
	  cp doc/$$i/*.htm $(DESTDIR)/doc/$$i/.; \
	done
	mkdir $(DESTDIR)/doc/images
	cp doc/images/*.jpg doc/images/gwlogo.gif $(DESTDIR)/doc/images/.
	mkdir $(DESTDIR)/lang
	cp hd/lang/*.txt $(DESTDIR)/lang/.
	for dir in \
	  af cn cs ct de dk en eo es et fi fr he is it lv nl no pt ru se;\
	do \
		mkdir $(DESTDIR)/lang/$$dir; \
		cp hd/lang/$$dir/start.txt $(DESTDIR)/lang/$$dir/.; \
	done
	mkdir $(DESTDIR)/images
	cp hd/images/*.jpg hd/images/*.gif $(DESTDIR)/images/.
	mkdir $(DESTDIR)/etc
	cp hd/etc/*.txt $(DESTDIR)/etc/.

clean::
	cd wserver; $(MAKE) clean
	cd src; $(MAKE) clean
	cd ged2gwb; $(MAKE) clean
	cd gwb2ged; $(MAKE) clean
	cd doc; $(MAKE) clean
	cd setup; $(MAKE) clean
	cd gwtp; $(MAKE) clean
	$(RM) -rf $(DESTDIR)
	$(RM) -f *~ .#*

depend:
	cd wserver; $(MAKE) depend
	cd src; $(MAKE) depend
	cd ged2gwb; $(MAKE) depend
	cd gwb2ged; $(MAKE) depend
	cd setup; $(MAKE) depend
	cd gwtp; $(MAKE) depend
