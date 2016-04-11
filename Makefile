ROOT := .

$(ROOT)/tools/Makefile.config:
	$(error Please run ./configure first)

include $(ROOT)/tools/Makefile.config

# ALL: directories needed to make distribution
ALL_TARGETS := wserver dag2html src ged2gwb gwb2ged setup gwtp
# EVERYTHING: any other maintained project
EVERYTHING_TARGETS := contrib/gwpublic contrib/oneshot contrib/misc contrib/gwFix contrib/history contrib/gwdiff

suffixed_TARGETS := $(foreach suffix,all clean depend everything opt out,$(ALL_TARGETS:=?$(suffix)) $(EVERYTHING_TARGETS:=?$(suffix)))

.PHONY: $(suffixed_TARGETS)

all depend everything opt out: $(DEPEND_DEPEND)
all: $(ALL_TARGETS:=?all)
clean: $(ALL_TARGETS:=?clean) $(EVERYTHING_TARGETS:=?clean)
depend: $(ALL_TARGETS:=?depend)
everything: $(ALL_TARGETS:=?everything) $(EVERYTHING_TARGETS:=?everything)
opt: $(ALL_TARGETS:=?opt)
out: $(ALL_TARGETS:=?out)

$(suffixed_TARGETS):
	$(MAKE) -C $(firstword $(subst ?, ,$@)) $(lastword $(subst ?, ,$@))

clean: clean-tmp
	$(RM) -r $(DESTDIR)

$(DEPEND_DEPEND):
	$(MAKE) -C $(dir $@) $(notdir $@)

.PHONY: install uninstall distrib

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
	cp setup/setup $(PREFIX)/bin/gwsetup$(EXE)
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
	cp setup/setup $(DESTDIR)/gw/gwsetup$(EXE)
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
