# $Id: Makefile,v 4.5 2001-06-03 14:42:54 ddr Exp $

DESTDIR=distribution
NS=tools/pa_newseq

include tools/Makefile.inc

all:: opt

out::
	$(MAKE) tools/pa_newseq.cmo
	cd wserver; $(MAKE) all
	cd dag2html; $(MAKE) out
	cd src; $(MAKE) all
	cd ged2gwb; $(MAKE) all
	cd gwb2ged; $(MAKE) all
	cd doc; $(MAKE) all
	cd setup; $(MAKE) all
	cd gwtp; $(MAKE) all

opt::
	$(MAKE) tools/pa_newseq.cmo
	cd wserver; $(MAKE) opt
	cd dag2html; $(MAKE) opt
	cd src; $(MAKE) opt
	cd ged2gwb; $(MAKE) opt
	cd gwb2ged; $(MAKE) opt
	cd doc; $(MAKE) opt
	cd setup; $(MAKE) opt
	cd gwtp; $(MAKE) opt

$(NS).cmo: $(NS).ml
	camlp4r pa_extend.cmo q_MLast.cmo pa_ifdef.cmo $(NS).ml -o $(NS).ppo
	$(OCAMLC) -I $(CAMLP4D) -c -impl $(NS).ppo
	$(RM) $*.ppo

distrib: new_distrib wrappers

wrappers:
	if test "$(CAMLP4F)" = "-DWIN95"; then \
	  echo 'cd gw' > $(DESTDIR)/gwd.bat; \
	  echo 'gwd' >> $(DESTDIR)/gwd.bat; \
	  echo 'cd gw' > $(DESTDIR)/gwsetup.bat; \
	  echo 'gwsetup' >> $(DESTDIR)/gwsetup.bat; \
	else \
	  (echo '#!/bin/sh'; \
	   echo 'mkdir -p bases'; \
	   echo 'cd bases'; \
	   echo 'exec ../gw/gwd -hd ../gw') > $(DESTDIR)/gwd; \
	  (echo '#!/bin/sh'; \
	   echo 'mkdir -p bases'; \
	   echo 'cd bases'; \
	   echo 'exec ../gw/gwsetup -gd ../gw') > $(DESTDIR)/gwsetup; \
	  chmod +x $(DESTDIR)/gwd $(DESTDIR)/gwsetup; \
	fi

new_distrib: classical_distrib
	mkdir t
	mv $(DESTDIR) t/gw
	mv t $(DESTDIR)
	mkdir $(DESTDIR)/gw/old
	mkdir $(DESTDIR)/gw/setup
	cp setup/intro.txt $(DESTDIR)/gw/setup/.
	for i in de en es fr lv sv; do \
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
	  cat etc/$$i.distrib.txt >> $(DESTDIR)/$$i.txt; \
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
	for i in de fr en nl sv; do \
	  mkdir $(DESTDIR)/doc/$$i; \
	  cp doc/$$i/*.htm $(DESTDIR)/doc/$$i/.; \
	done
	mkdir $(DESTDIR)/doc/images
	cp doc/images/*.jpg doc/images/gwlogo.gif $(DESTDIR)/doc/images/.
	mkdir $(DESTDIR)/lang
	cp hd/lang/*.txt $(DESTDIR)/lang/.
	mkdir $(DESTDIR)/images
	cp hd/images/*.jpg hd/images/*.gif $(DESTDIR)/images/.
	mkdir $(DESTDIR)/etc
	cp hd/etc/*.txt $(DESTDIR)/etc/.

clean::
	$(RM) $(NS).cm[oi]
	cd wserver; $(MAKE) clean
	cd dag2html; $(MAKE) clean
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
