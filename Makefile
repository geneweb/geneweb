# $Id: Makefile,v 5.7 2009-03-11 10:53:30 ddr Exp $

PREFIX=/usr
LANGDIR=$(PREFIX)/share/geneweb
MANDIR=$(PREFIX)/man/man1
DESTDIR=distribution
MANPAGES=ged2gwb.1 gwb2ged.1 gwc.1 gwc2.1 gwu.1 gwd.1 consang.1 gwsetup.1

include tools/Makefile.inc

all:: opt

out::
	cd wserver; $(MAKE) all
	cd dag2html; $(MAKE) out
	cd src; $(MAKE) PREFIX=$(PREFIX) all
	cd ged2gwb; $(MAKE) all
	cd gwb2ged; $(MAKE) all
	cd setup; $(MAKE) all
	cd gwtp; $(MAKE) all

opt::
	cd wserver; $(MAKE) opt
	cd dag2html; $(MAKE) opt
	cd src; $(MAKE) PREFIX=$(PREFIX) opt
	cd ged2gwb; $(MAKE) opt
	cd gwb2ged; $(MAKE) opt
	cd setup; $(MAKE) opt
	cd gwtp; $(MAKE) opt

install:
	mkdir -p $(PREFIX)/bin
	cp src/gwc $(PREFIX)/bin/gwc$(EXE)
	cp src/gwc1 $(PREFIX)/bin/gwc1$(EXE)
	cp src/gwc2 $(PREFIX)/bin/gwc2$(EXE)
	cp src/consang $(PREFIX)/bin/consang$(EXE)
	cp src/gwd $(PREFIX)/bin/gwd$(EXE)
	cp src/gwu $(PREFIX)/bin/gwu$(EXE)
	cp ged2gwb/ged2gwb $(PREFIX)/bin/ged2gwb$(EXE)
	cp ged2gwb/ged2gwb2 $(PREFIX)/bin/ged2gwb2$(EXE)
	cp gwb2ged/gwb2ged $(PREFIX)/bin/gwb2ged$(EXE)
	mkdir -p $(LANGDIR)/lang
	cp hd/lang/*.txt $(LANGDIR)/lang/.
	mkdir -p $(LANGDIR)/images
	mkdir -p $(LANGDIR)/images/flags
	cp hd/images/flags/*.jpg hd/images/flags/*.png $(LANGDIR)/images/flags/.
	cp hd/images/*.jpg hd/images/*.png hd/images/*.ico $(LANGDIR)/images/.
	mkdir -p $(LANGDIR)/etc
	cp -R hd/etc/* $(LANGDIR)/etc/.
	find $(LANGDIR)/etc/ -name .svn -type d -prune -exec rm -rf {} \;
	mkdir -p $(MANDIR)
	cd man; cp $(MANPAGES) $(MANDIR)/.

uninstall:
	rm -f $(PREFIX)/bin/gwc$(EXE)
	rm -f $(PREFIX)/bin/gwc1$(EXE)
	rm -f $(PREFIX)/bin/gwc2$(EXE)
	rm -f $(PREFIX)/bin/consang$(EXE)
	rm -f $(PREFIX)/bin/gwd$(EXE)
	rm -f $(PREFIX)/bin/gwu$(EXE)
	rm -f $(PREFIX)/bin/ged2gwb$(EXE)
	rm -f $(PREFIX)/bin/gwb2ged$(EXE)
	rm -rf $(PREFIX)/share/geneweb
	cd $(MANDIR); rm -f $(MANPAGES)

distrib: new_distrib wrappers

wrappers:
	if test "$(CAMLP5F)" = "-DWIN95"; then \
	  echo -ne 'setlocal enableextensions\r\n' > $(DESTDIR)/gwd.bat; \
	  echo -ne 'md bases\r\n' >> $(DESTDIR)/gwd.bat; \
	  echo -ne 'endlocal\r\n' >> $(DESTDIR)/gwd.bat; \
	  echo -ne 'cd bases\r\n' >> $(DESTDIR)/gwd.bat; \
	  echo -ne 'start /MIN ..\\gw\\gwd -hd ..\\gw\r\n' >> $(DESTDIR)/gwd.bat; \
	  echo -ne 'setlocal enableextensions\r\n' > $(DESTDIR)/gwsetup.bat; \
	  echo -ne 'md bases\r\n' >> $(DESTDIR)/gwsetup.bat; \
	  echo -ne 'endlocal\r\n' >> $(DESTDIR)/gwsetup.bat; \
	  echo -ne 'cd bases\r\n' >> $(DESTDIR)/gwsetup.bat; \
	  echo -ne 'start /MIN ..\\gw\\gwsetup -lang fr -gd ..\\gw\r\n' >> $(DESTDIR)/gwsetup.bat; \
	else \
	  (echo '#!/bin/sh'; \
	   echo 'cd `dirname $$0`'; \
	   echo 'mkdir -p bases'; \
	   echo 'cd bases'; \
	   echo 'exec ../gw/gwd -hd ../gw "$$@"') > $(DESTDIR)/gwd; \
	  (echo '#!/bin/sh'; \
	   echo 'cd `dirname $$0`'; \
	   echo 'mkdir -p bases'; \
	   echo 'cd bases'; \
	   echo 'exec ../gw/gwsetup -gd ../gw "$$@"') > $(DESTDIR)/gwsetup; \
	  chmod +x $(DESTDIR)/gwd $(DESTDIR)/gwsetup; \
	fi
	if test $(shell uname -s) = "Darwin"; then \
	  cp etc/MacOSX/GeneWeb.command $(DESTDIR); \
	fi

new_distrib: classical_distrib
	mkdir t
	mv $(DESTDIR) t/gw
	mv t $(DESTDIR)
	mkdir $(DESTDIR)/gw/old
	mkdir $(DESTDIR)/gw/setup
	cp setup/intro.txt $(DESTDIR)/gw/setup/.
	mkdir $(DESTDIR)/gw/setup/lang
	if test "$(CAMLP5F)" = "-DWIN95"; then \
	  cp setup/lang/intro.txt.dos $(DESTDIR)/gw/setup/lang/intro.txt; \
	else \
	  cp setup/lang/intro.txt $(DESTDIR)/gw/setup/lang/intro.txt; \
	fi
	cp setup/lang/*.htm $(DESTDIR)/gw/setup/lang/.
	cp setup/lang/lexicon.txt $(DESTDIR)/gw/setup/lang/.
	cp setup/gwsetup $(DESTDIR)/gw/gwsetup$(EXE)
	cp LICENSE $(DESTDIR)/LICENSE.txt
	cp etc/START.htm $(DESTDIR)/.
	cp CHANGES $(DESTDIR)/CHANGES.txt
	echo "127.0.0.1" > $(DESTDIR)/gw/only.txt
	echo "-setup_link" > $(DESTDIR)/gw/gwd.arg

classical_distrib:
	$(RM) -rf $(DESTDIR)
	mkdir $(DESTDIR)
	cp src/gwc $(DESTDIR)/gwc$(EXE)
	cp src/gwc1 $(DESTDIR)/gwc1$(EXE)
	cp src/gwc2 $(DESTDIR)/gwc2$(EXE)
	cp src/consang $(DESTDIR)/consang$(EXE)
	cp src/gwd $(DESTDIR)/gwd$(EXE)
	cp src/gwu $(DESTDIR)/gwu$(EXE)
	cp src/update_nldb $(DESTDIR)/update_nldb$(EXE)
	cp ged2gwb/ged2gwb $(DESTDIR)/ged2gwb$(EXE)
	cp ged2gwb/ged2gwb2 $(DESTDIR)/ged2gwb2$(EXE)
	cp gwb2ged/gwb2ged $(DESTDIR)/gwb2ged$(EXE)
	mkdir $(DESTDIR)/gwtp_tmp
	mkdir $(DESTDIR)/gwtp_tmp/lang
	cp gwtp/gwtp $(DESTDIR)/gwtp_tmp/gwtp$(EXE)
	cp gwtp/README $(DESTDIR)/gwtp_tmp/.
	cp gwtp/lang/*.txt $(DESTDIR)/gwtp_tmp/lang/.
	cp etc/a.gwf $(DESTDIR)/.
	mkdir $(DESTDIR)/lang
	cp hd/lang/*.txt $(DESTDIR)/lang/.
	mkdir $(DESTDIR)/images
	mkdir $(DESTDIR)/images/flags
	cp hd/images/flags/*.jpg hd/images/flags/*.png $(DESTDIR)/images/flags/.
	cp hd/images/*.jpg hd/images/*.png hd/images/*.ico $(DESTDIR)/images/.
	mkdir $(DESTDIR)/etc
	cp -R hd/etc/* $(DESTDIR)/etc/.
	find $(DESTDIR)/etc/ -name .svn -type d -prune -exec rm -rf {} \;

windows_files:
	@for i in distribution/*.txt distribution/gw/*.txt; do \
	  echo "========================================="; \
	  echo $$i; \
	  cp $$i $$i~; \
	  sed -e 's/$$/\r/' $$i~ > $$i; \
	  rm $$i~; \
	done

clean::
	cd wserver; $(MAKE) clean
	cd dag2html; $(MAKE) clean
	cd src; $(MAKE) clean
	cd ged2gwb; $(MAKE) clean
	cd gwb2ged; $(MAKE) clean
	cd setup; $(MAKE) clean
	cd gwtp; $(MAKE) clean
	$(RM) -rf $(DESTDIR)
	$(RM) -f *~ .#*

clean_mismatch:
	rm src/pa_lock.cmo src/pa_html.cmo src/def_syn.cmo

depend:
	cd src; $(MAKE) pr_dep.cmo def_syn.cmo gwlib.ml
	cd src; $(MAKE) pa_lock.cmo pa_html.cmo q_codes.cmo
	cd wserver; $(MAKE) depend
	cd src; $(MAKE) depend
	cd ged2gwb; $(MAKE) depend
	cd gwb2ged; $(MAKE) depend
	cd setup; $(MAKE) depend
	cd gwtp; $(MAKE) depend
