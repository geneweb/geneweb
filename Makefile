Makefile.config: configure
	@if [ -e "$@" ]; then \
	  echo "configure file has changed. Please rerun ./configure"; exit 1; \
	else \
	  echo "Please run ./configure first"; exit 1; \
	fi

include Makefile.config
-include Makefile.local

# Variables for packagers.
PREFIX=/usr
LANGDIR=$(PREFIX)/share/geneweb
MANDIR=$(PREFIX)/man/man1
DESTDIR=distribution
MANPAGES=ged2gwb.1 gwb2ged.1 gwc.1 gwc2.1 gwu.1 gwd.1 consang.1 gwsetup.1

.DEFAULT_GOAL = exe

BUILD_DIR=_build/default/

# Executables

INSTALL_EXE = \
	src/gwc1 \
	src/gwc2 \
	src/mk_consang \
	src/gwd \
	src/gwu \
	src/update_nldb \
	ged2gwb/ged2gwb \
	ged2gwb/ged2gwb2 \
	gwb2ged/gwb2ged \
	setup/setup

DISTRIB_EXE = \
	$(INSTALL_EXE) \


ALL_EXE = \
	$(INSTALL_EXE) \
	gwtp/gwtp \
	gui/gui \
	contrib/gwpublic/gwpublic1 \
	contrib/gwpublic/gwpublic2 \
	contrib/gwpublic/gwpublic2priv \
	contrib/gwpublic/gwprivate \
	contrib/gwpublic/gwiftitles \
	contrib/gwFix/gwFixBase \
	contrib/gwFix/gwFixFromFile \
	contrib/gwFix/gwFixFromFileDomicile \
	contrib/gwFix/gwFixFromFileAlias \
	contrib/gwFix/gwFixBurial \
	contrib/gwFix/gwFixEvtSrc \
	contrib/gwFix/gwFixColon \
	contrib/gwFix/gwFindCpl \
	contrib/gwFix/gwFixY \
	contrib/gwdiff/gwdiff \
	contrib/gwbase/etc/public \
	contrib/gwbase/etc/public2 \
	contrib/history/convert_hist \
	contrib/history/fix_hist \
	contrib/history/is_gw_plus \
	contrib/lex/lex_utils \
	contrib/misc/lower_string \
	contrib/oneshot/gwRemoveImgGallery \
	contrib/oneshot/gwBaseCompatiblePlus \
	contrib/oneshot/gwFixDateText \
	contrib/oneshot/gwExportAscCSV

EVERYTHING_EXE = \
	$(ALL_EXE) \
	contrib/gwbase/etc/geneanet \
	contrib/gwbase/etc/clavier \
	contrib/gwbase/etc/connex \
	contrib/gwbase/etc/hist \
	contrib/gwbase/etc/selroy \
	contrib/gwbase/etc/chkimg \
	contrib/gwbase/etc/consmoy \
	contrib/gwbase/etc/lune \
	contrib/gwbase/etc/titres \
	contrib/gwbase/etc/gwck \
	contrib/gwbase/etc/nbdesc \
	contrib/gwbase/etc/probot \
	contrib/oneshot/gwFixAddEvent \
	contrib/oneshot/gwMostAsc \
	dag2html/main \
	gwtp/recover \
	src/check_base \
	src/i18n_check

CAMLP5_PA_EXTEND_FILES = \
	ged2gwb/ged2gwb \
	ged2gwb/ged2gwb2 \
	src/pr_transl \
	lib/templ \
	lib/update \
	setup/setup

CAMLP5_Q_MLAST_FILES = \
	src/pr_transl \
	lib/templ

CAMLP5_FILES = $(sort $(CAMLP5_Q_MLAST_FILES) $(CAMLP5_PA_EXTEND_FILES))

$(CAMLP5_PA_EXTEND_FILES:=.ml): CAMLP5_OPT += pa_extend.cmo
$(CAMLP5_Q_MLAST_FILES:=.ml): CAMLP5_OPT += q_MLast.cmo

%.ml: CAMLP5_OPT=

%.ml: %.camlp5
	@([ -z "$(CAMLP5_OPT)" ] \
	|| false \
	&& echo "ERROR generating $@: CAMLP5_OPT variable must be defined") \
	|| (echo -n "Generating $@..." \
	    && echo "(* DO NOT EDIT *)" > $@ \
	    && echo "(* This file was generated from $< *)" >> $@ \
	    && camlp5o pr_o.cmo $(CAMLP5_OPT) -impl $< >> $@ \
	    && echo " Done!")

%.bc %.exe:
	dune build $@

install-exe: lib/gwlib.ml $(CAMLP5_FILES:=.camlp5) $(CAMLP5_FILES:=.ml) $(INSTALL_EXE:=.exe)
install-bc: lib/gwlib.ml $(CAMLP5_FILES:=.camlp5) $(CAMLP5_FILES:=.ml) $(INSTALL_EXE:=.bc)

exe: install-exe $(ALL_EXE:=.exe)
bc: install-bc $(ALL_EXE:=.bc)

everything-bc: bc $(EVERYTHING_EXE:=.bc)
everything-exe: exe $(EVERYTHING_EXE:=.exe)

clean:
	$(RM) $(CAMLP5_FILES:=.ml) lib/gwlib.ml
	$(RM)r distribution
	dune clean

lib/gwlib.ml:
	echo "let prefix =" > $@
	echo "  try Sys.getenv \"GWPREFIX\"" >> $@
	echo "  with Not_found -> \"$(PREFIX)\"" | sed -e 's|\\|/|g' >> $@

.PHONY: piqi
piqi:
	$(foreach p, $(wildcard src/*.proto), \
		piqi of-proto --normalize $(p) ; \
		piqic-ocaml -C src/ --ext $(p).piqi ; \
	  )
	$(RM) src/*.piqi

.PHONY: install uninstall distrib

install: install-exe
	PWD=`pwd`
	if test "$(OS_TYPE)" = "Darwin"; then \
	    etc/macOS/install.command $(PWD) $(DESTDIR) etc/macOS; \
	elif test "$(OS_TYPE)" = "Win"; then \
		echo "No install for Window"; \
	else \
	mkdir -p $(PREFIX)/bin; \
	cp $(BUILD_DIR)/src/gwc1.exe $(PREFIX)/bin/gwc$(EXE); \
	cp $(BUILD_DIR)/src/gwc1.exe $(PREFIX)/bin/gwc1$(EXE); \
	cp $(BUILD_DIR)/src/gwc2.exe $(PREFIX)/bin/gwc2$(EXE); \
	cp $(BUILD_DIR)/src/mk_consang.exe $(PREFIX)/bin/mk_consang$(EXE); \
	cp $(BUILD_DIR)/src/mk_consang.exe $(PREFIX)/bin/consang$(EXE); \
	cp $(BUILD_DIR)/src/gwd.exe $(PREFIX)/bin/gwd$(EXE); \
	cp $(BUILD_DIR)/src/gwu.exe $(PREFIX)/bin/gwu$(EXE); \
	cp $(BUILD_DIR)/ged2gwb/ged2gwb.exe $(PREFIX)/bin/ged2gwb$(EXE); \
	cp $(BUILD_DIR)/ged2gwb/ged2gwb2.exe $(PREFIX)/bin/ged2gwb2$(EXE); \
	cp $(BUILD_DIR)/gwb2ged/gwb2ged.exe $(PREFIX)/bin/gwb2ged$(EXE); \
	cp $(BUILD_DIR)/setup/setup.exe $(PREFIX)/bin/gwsetup$(EXE); \
	cp $(BUILD_DIR)/src/update_nldb.exe $(PREFIX)/bin/update_nldb$(EXE); \
	mkdir -p $(LANGDIR); \
	cp -R hd/* $(LANGDIR)/.; \
	mkdir -p $(MANDIR); \
	cd man; cp $(MANPAGES) $(MANDIR)/.; \
	fi

uninstall:
	$(RM) $(addprefix $(PREFIX)/bin/,$(INSTALL_EXE))
	$(RM) -r $(PREFIX)/share/geneweb
	cd $(MANDIR); $(RM) $(MANPAGES)

distribution: distrib
distrib: install-exe gwtp/gwtp.exe
	$(RM) -r $(DESTDIR)
	mkdir $(DESTDIR)
	mkdir -p $(DESTDIR)/bases
	cp CHANGES $(DESTDIR)/CHANGES.txt
	cp LICENSE $(DESTDIR)/LICENSE.txt
	cp etc/README.txt $(DESTDIR)/.
	cp etc/LISEZMOI.txt $(DESTDIR)/.
	cp etc/START.htm $(DESTDIR)/.
	if test $(OS_TYPE) = "Win"; then \
	  cp etc/Windows/gwd.bat $(DESTDIR); \
	  cp etc/Windows/gwsetup.bat $(DESTDIR); \
	  cp -f etc/Windows/README.txt $(DESTDIR)/README.txt; \
	  cp -f etc/Windows/LISEZMOI.txt $(DESTDIR)/LISEZMOI.txt; \
	elif test $(OS_TYPE) = "Darwin"; then \
	  cp etc/gwd $(DESTDIR)/gwd.command; \
	  cp etc/gwsetup $(DESTDIR)/gwsetup.command; \
	  cp etc/macOS/geneweb.command $(DESTDIR); \
	else \
	  cp etc/gwd $(DESTDIR); \
	  cp etc/gwsetup $(DESTDIR); \
	fi
	mkdir $(DESTDIR)/gw
	cp etc/a.gwf $(DESTDIR)/gw/.
	echo "127.0.0.1" > $(DESTDIR)/gw/only.txt
	echo "-setup_link" > $(DESTDIR)/gw/gwd.arg
	cp $(BUILD_DIR)/src/gwc1.exe $(DESTDIR)/gw/gwc$(EXE)
	cp $(BUILD_DIR)/src/gwc1.exe $(DESTDIR)/gw/gwc1$(EXE)
	cp $(BUILD_DIR)/src/gwc2.exe $(DESTDIR)/gw/gwc2$(EXE)
	cp $(BUILD_DIR)/src/mk_consang.exe $(DESTDIR)/gw/mk_consang$(EXE)
	cp $(BUILD_DIR)/src/mk_consang.exe $(DESTDIR)/gw/consang$(EXE)
	cp $(BUILD_DIR)/src/gwd.exe $(DESTDIR)/gw/gwd$(EXE)
	cp $(BUILD_DIR)/src/gwu.exe $(DESTDIR)/gw/gwu$(EXE)
	cp $(BUILD_DIR)/src/update_nldb.exe $(DESTDIR)/gw/update_nldb$(EXE)
	cp $(BUILD_DIR)/ged2gwb/ged2gwb.exe $(DESTDIR)/gw/ged2gwb$(EXE)
	cp $(BUILD_DIR)/ged2gwb/ged2gwb2.exe $(DESTDIR)/gw/ged2gwb2$(EXE)
	cp $(BUILD_DIR)/gwb2ged/gwb2ged.exe $(DESTDIR)/gw/gwb2ged$(EXE)
	cp $(BUILD_DIR)/setup/setup.exe $(DESTDIR)/gw/gwsetup$(EXE)
	mkdir $(DESTDIR)/gw/gwtp_tmp
	mkdir $(DESTDIR)/gw/gwtp_tmp/lang
	cp gwtp/README $(DESTDIR)/gw/gwtp_tmp/.
	cp $(BUILD_DIR)/gwtp/gwtp.exe $(DESTDIR)/gw/gwtp_tmp/gwtp$(EXE)
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
	rm -f $(DESTDIR)/exe-version.txt
	echo "Commit: `git log  -1 | grep commit | cut -c8-15`, `date`"      > $(DESTDIR)/commit.txt
	echo "`ocaml  -version`"      >> $(DESTDIR)/commit.txt
	echo "`camlp5 -v 2>&1`"       >> $(DESTDIR)/commit.txt
	echo "-----"                  >> $(DESTDIR)/commit.txt
