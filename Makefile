ROOT=.
include $(ROOT)/tools/Makefile.config

# First rule 'all' is defined in Makefile.config.

.PHONY: out opt install uninstall distrib clean depend

out:
	cd wserver && $(MAKE) $@
	cd dag2html && $(MAKE) $@
	cd src && $(MAKE) $@
	cd ged2gwb && $(MAKE) $@
	cd gwb2ged && $(MAKE) $@
	cd setup && $(MAKE) $@
	cd gwtp && $(MAKE) $@

opt:
	cd wserver && $(MAKE) $@
	cd dag2html && $(MAKE) $@
	cd src && $(MAKE) $@
	cd ged2gwb && $(MAKE) $@
	cd gwb2ged && $(MAKE) $@
	cd setup && $(MAKE) $@
	cd gwtp && $(MAKE) $@
	cd contrib/gwpublic && $(MAKE) all
	cd contrib/oneshot && $(MAKE) all
	cd contrib/misc && $(MAKE) all
	cd contrib/gwFix && $(MAKE) all
	cd contrib/history && $(MAKE) all

install:
	mkdir -p $(PREFIX)/bin
	cp src/gwc1 $(PREFIX)/bin/gwc$(EXE)
	cp src/gwc1 $(PREFIX)/bin/gwc1$(EXE)
	cp src/gwc2 $(PREFIX)/bin/gwc2$(EXE)
	cp src/consang $(PREFIX)/bin/consang$(EXE)
	cp src/gwd $(PREFIX)/bin/gwd$(EXE)
	cp src/gwu $(PREFIX)/bin/gwu$(EXE)
	cp ged2gwb/ged2gwb $(PREFIX)/bin/ged2gwb$(EXE)
	cp ged2gwb/ged2gwb2 $(PREFIX)/bin/ged2gwb2$(EXE)
	cp gwb2ged/gwb2ged $(PREFIX)/bin/gwb2ged$(EXE)
	cp setup/gwsetup $(PREFIX)/bin/gwsetup$(EXE)
	cp src/update_nldb $(PREFIX)/bin/update_nldb$(EXE)
	cp -R hd/* $(LANGDIR)/.
	mkdir -p $(MANDIR)
	cd man; cp $(MANPAGES) $(MANDIR)/.

uninstall:
	$(RM) $(PREFIX)/bin/gwc$(EXE)
	$(RM) $(PREFIX)/bin/gwc1$(EXE)
	$(RM) $(PREFIX)/bin/gwc2$(EXE)
	$(RM) $(PREFIX)/bin/consang$(EXE)
	$(RM) $(PREFIX)/bin/gwd$(EXE)
	$(RM) $(PREFIX)/bin/gwu$(EXE)
	$(RM) $(PREFIX)/bin/ged2gwb$(EXE)
	$(RM) $(PREFIX)/bin/gwb2ged$(EXE)
	$(RM) $(PREFIX)/bin/gwsetup$(EXE)
	$(RM) $(PREFIX)/bin/update_nldb$(EXE)
	$(RM) -r $(PREFIX)/share/geneweb
	cd $(MANDIR); $(RM) $(MANPAGES)

distrib:
	$(RM) -r $(DESTDIR)
	mkdir $(DESTDIR)
	cp CHANGES $(DESTDIR)/CHANGES.txt
	cp LICENSE $(DESTDIR)/LICENSE.txt
	cp etc/START.htm $(DESTDIR)/.
	if test $(OS_TYPE) = "Win"; then \
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
	   echo 'mkdir -p bases'; \
	   echo 'cd bases'; \
	   echo 'exec ../gw/gwd -hd ../gw "$$@"') > $(DESTDIR)/gwd; \
	  (echo '#!/bin/sh'; \
	   echo 'mkdir -p bases'; \
	   echo 'cd bases'; \
	   echo 'exec ../gw/gwsetup -gd ../gw "$$@"') > $(DESTDIR)/gwsetup; \
	  chmod +x $(DESTDIR)/gwd $(DESTDIR)/gwsetup; \
	fi
	if test $(OS_TYPE) = "Darwin"; then \
	  cp etc/MacOSX/GeneWeb.command $(DESTDIR); \
	  chmod +x $(DESTDIR)/GeneWeb.command; \
	fi
	mkdir $(DESTDIR)/gw
	cp etc/a.gwf $(DESTDIR)/gw/.
	echo "127.0.0.1" > $(DESTDIR)/gw/only.txt
	echo "-setup_link" > $(DESTDIR)/gw/gwd.arg
	cp src/gwc1 $(DESTDIR)/gw/gwc$(EXE)
	cp src/gwc1 $(DESTDIR)/gw/gwc1$(EXE)
	cp src/gwc2 $(DESTDIR)/gw/gwc2$(EXE)
	cp src/consang $(DESTDIR)/gw/consang$(EXE)
	cp src/gwd $(DESTDIR)/gw/gwd$(EXE)
	cp src/gwu $(DESTDIR)/gw/gwu$(EXE)
	cp src/update_nldb $(DESTDIR)/gw/update_nldb$(EXE)
	cp ged2gwb/ged2gwb $(DESTDIR)/gw/ged2gwb$(EXE)
	cp ged2gwb/ged2gwb2 $(DESTDIR)/gw/ged2gwb2$(EXE)
	cp gwb2ged/gwb2ged $(DESTDIR)/gw/gwb2ged$(EXE)
	cp setup/gwsetup $(DESTDIR)/gw/gwsetup$(EXE)
	mkdir $(DESTDIR)/gw/gwtp_tmp
	mkdir $(DESTDIR)/gw/gwtp_tmp/lang
	cp gwtp/README $(DESTDIR)/gw/gwtp_tmp/.
	cp gwtp/gwtp $(DESTDIR)/gw/gwtp_tmp/gwtp$(EXE)
	cp gwtp/lang/*.txt $(DESTDIR)/gw/gwtp_tmp/lang/.
	mkdir $(DESTDIR)/gw/setup
	cp setup/intro.txt $(DESTDIR)/gw/setup/.
	mkdir $(DESTDIR)/gw/setup/lang
	if test $(OS_TYPE) = "Win"; then \
	  cp setup/lang/intro.txt.dos $(DESTDIR)/gw/setup/lang/intro.txt; \
	else \
	  cp setup/lang/intro.txt $(DESTDIR)/gw/setup/lang/intro.txt; \
	fi
	cp setup/lang/*.htm $(DESTDIR)/gw/setup/lang/.
	cp setup/lang/lexicon.txt $(DESTDIR)/gw/setup/lang/.
	cp -R hd/* $(DESTDIR)/gw/.

clean:
	cd wserver && $(MAKE) $@
	cd dag2html && $(MAKE) $@
	cd src && $(MAKE) $@
	cd ged2gwb && $(MAKE) $@
	cd gwb2ged && $(MAKE) $@
	cd setup && $(MAKE) $@
	cd gwtp && $(MAKE) $@
	cd contrib/gwpublic && $(MAKE) $@
	cd contrib/oneshot && $(MAKE) $@
	cd contrib/misc && $(MAKE) $@
	cd contrib/gwFix && $(MAKE) $@
	cd contrib/history && $(MAKE) $@
	$(RM) -r $(DESTDIR)
	$(RM) *~ .#*

depend:
	cd src && $(MAKE) pa_lock.cmo pa_html.cmo q_codes.cmo
	cd wserver && $(MAKE) $@
	cd dag2html && $(MAKE) $@
	cd src && $(MAKE) $@
	cd ged2gwb && $(MAKE) $@
	cd gwb2ged && $(MAKE) $@
	cd setup && $(MAKE) $@
	cd gwtp && $(MAKE) $@
