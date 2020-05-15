ifneq ($(MAKECMDGOALS),ci)
Makefile.config: configure.ml
	@if [ -e "$@" ]; then \
	  echo "configure file has changed. Please rerun ocaml ./configure.ml"; exit 1; \
	else \
	  echo "Please run ocaml ./configure.ml first"; exit 1; \
	fi
include Makefile.config
endif

-include Makefile.local

# Variables for packagers.
PREFIX=/usr
DISTRIB_DIR=distribution

BUILD_DIR=_build/default

EXE = \
	bin/connex.exe \
	bin/consang.exe \
	bin/ged2gwb.exe \
	bin/gwb2ged.exe \
	bin/gwc.exe \
	bin/gwd.exe \
	bin/gwdiff.exe \
	bin/gwfixbase.exe \
	bin/gwu.exe \
	bin/setup.exe \
	bin/update_nldb.exe \

###### [BEGIN] Generated files section

CAMLP5_PA_EXTEND_FILES = \
	lib/templ \
	bin/setup/setup

CAMLP5_Q_MLAST_FILES = \
	lib/templ

CAMLP5_FILES = $(sort $(CAMLP5_Q_MLAST_FILES) $(CAMLP5_PA_EXTEND_FILES))

$(CAMLP5_PA_EXTEND_FILES:=.ml): CAMLP5_OPT += pa_extend.cmo
$(CAMLP5_Q_MLAST_FILES:=.ml): CAMLP5_OPT += q_MLast.cmo

%.ml: CAMLP5_OPT=

%.ml: %.camlp5.ml
	@([ -z "$(CAMLP5_OPT)" ] \
	|| false \
	&& echo "ERROR generating $@: CAMLP5_OPT variable must be defined") \
	|| (echo -n "Generating $@..." \
	    && echo "(* DO NOT EDIT *)" > $@ \
	    && echo "(* This file was generated from $< *)" >> $@ \
	    && camlp5o pr_o.cmo $(CAMLP5_OPT) -impl $< >> $@ \
	    && sed -i.bak -E 's/[(]\* (\[@.+\]) \*[)]/\1/g' $@ \
	    && sed -i.bak -E 's/[(]\* (#[^\*]+) \*[)]/\1/g' $@ \
	    && rm $@.bak \
	    && echo " Done!")

lib/gwlib.ml:
	@echo -n "Generating $@..."
	@echo "let prefix =" > $@
	@echo "  try Sys.getenv \"GWPREFIX\"" >> $@
	@echo "  with Not_found -> \"$(PREFIX)\"" | sed -e 's|\\|/|g' >> $@
	@echo " Done!"

CPPO_D=$(API_D)

%/dune: %/dune.in Makefile.config
	@echo -n "Generating $@..." \
	&& cat $< \
	| cppo -n $(CPPO_D) \
	| sed \
	-e "s/%%%CPPO_D%%%/$(CPPO_D)/g" \
	-e "s/%%%API_PKG%%%/$(API_PKG)/g" \
	-e "s/%%%SOSA_PKG%%%/$(SOSA_PKG)/g" \
	-e "s/%%%GWDB_PKG%%%/$(GWDB_PKG)/g" \
	-e "s/%%%WSERVER_PKG%%%/$(WSERVER_PKG)/g" \
	-e "s/%%%DUNE_DIRS_EXCLUDE%%%/$(DUNE_DIRS_EXCLUDE)/g" \
	> $@ \
	&& echo " Done!"

dune-workspace: dune-workspace.in Makefile.config
	cat $< | sed  -e "s/%%%DUNE_PROFILE%%%/$(DUNE_PROFILE)/g" > $@

hd/etc/version.txt:
	@echo -n "Generating $@..."
	@echo "GeneWeb[:] [compiled on %s from commit %s:::" > $@
	@echo "$$(date '+%Y-%m-%d'):" >> $@
	@echo "$$(git show -s --date=short --pretty=format:'<a href="https://github.com/geneweb/geneweb/commit/%h">%h (%cd)</a>')]" >> $@
	@echo " Done!"
.PHONY:hd/etc/version.txt

###### [End] Generated files section

GENERATED_FILES_DEP = \
	hd/etc/version.txt \
	lib/gwlib.ml \
	$(CAMLP5_FILES:=.ml) \
	benchmark/dune \
	bin/dune \
	lib/dune \
	test/dune \
	dune-workspace \

ifdef API_D
piqi:
	$(foreach p, $(wildcard lib/*.proto), \
		piqi of-proto --normalize $(p) ; \
		piqic-ocaml -C lib/ --ext $(p).piqi ; \
	  )
	$(RM) lib/*.piqi
else
piqi:
endif
.PHONY: piqi

%.exe: | piqi $(GENERATED_FILES_DEP)
	dune build $@
exe:
	dune build $(ALL_EXE:=.exe)
.DEFAULT_GOAL = exe

install uninstall exe: $(GENERATED_FILES_DEP) piqi

###### [BEGIN] Installation / Distribution section

install:
	dune build @install
	dune install

uninstall:
	dune build @install
	dune uninstall

BUILD_DISTRIB_DIR=$(BUILD_DIR)/bin/

distrib: exe
	$(RM) -r $(DISTRIB_DIR)
	mkdir $(DISTRIB_DIR)
	mkdir -p $(DISTRIB_DIR)/bases
	cp CHANGES $(DISTRIB_DIR)/CHANGES.txt
	cp LICENSE $(DISTRIB_DIR)/LICENSE.txt
	cp etc/README.txt $(DISTRIB_DIR)/.
	cp etc/LISEZMOI.txt $(DISTRIB_DIR)/.
	cp etc/START.htm $(DISTRIB_DIR)/.
	if test $(OS_TYPE) = "Win"; then \
	  cp etc/Windows/gwd.bat $(DISTRIB_DIR); \
	  cp etc/Windows/gwsetup.bat $(DISTRIB_DIR); \
	  cp -f etc/Windows/README.txt $(DISTRIB_DIR)/README.txt; \
	  cp -f etc/Windows/LISEZMOI.txt $(DISTRIB_DIR)/LISEZMOI.txt; \
	elif test $(OS_TYPE) = "Darwin"; then \
	  cp etc/gwd $(DISTRIB_DIR)/gwd.command; \
	  cp etc/gwsetup $(DISTRIB_DIR)/gwsetup.command; \
	  cp etc/macOS/geneweb.command $(DISTRIB_DIR); \
	else \
	  cp etc/gwd $(DISTRIB_DIR); \
	  cp etc/gwsetup $(DISTRIB_DIR); \
	fi
	mkdir $(DISTRIB_DIR)/gw
	cp etc/a.gwf $(DISTRIB_DIR)/gw/.
	echo "127.0.0.1" > $(DISTRIB_DIR)/gw/only.txt
	echo "-setup_link" > $(DISTRIB_DIR)/gw/gwd.arg
	cp $(BUILD_DISTRIB_DIR)connex.exe $(DISTRIB_DIR)/gw/connex$(EXT);
	cp $(BUILD_DISTRIB_DIR)consang.exe $(DISTRIB_DIR)/gw/consang$(EXT);
	cp $(BUILD_DISTRIB_DIR)ged2gwb.exe $(DISTRIB_DIR)/gw/ged2gwb$(EXT);
	cp $(BUILD_DISTRIB_DIR)gwb2ged.exe $(DISTRIB_DIR)/gw/gwb2ged$(EXT);
	cp $(BUILD_DISTRIB_DIR)gwc.exe $(DISTRIB_DIR)/gw/gwc$(EXT);
	cp $(BUILD_DISTRIB_DIR)gwd.exe $(DISTRIB_DIR)/gw/gwd$(EXT);
	cp $(BUILD_DISTRIB_DIR)gwdiff.exe $(DISTRIB_DIR)/gw/gwdiff$(EXT);
	cp $(BUILD_DISTRIB_DIR)gwu.exe $(DISTRIB_DIR)/gw/gwu$(EXT);
	cp $(BUILD_DISTRIB_DIR)gwfixbase.exe $(DISTRIB_DIR)/gw/gwfixbase$(EXT);
	cp $(BUILD_DISTRIB_DIR)setup.exe $(DISTRIB_DIR)/gw/gwsetup$(EXT);
	cp $(BUILD_DISTRIB_DIR)update_nldb.exe $(DISTRIB_DIR)/gw/update_nldb$(EXT);
	mkdir $(DISTRIB_DIR)/gw/setup
	cp bin/setup/intro.txt $(DISTRIB_DIR)/gw/setup/
	mkdir $(DISTRIB_DIR)/gw/setup/lang
	cp bin/setup/setup.gwf $(DISTRIB_DIR)/gw/setup/
	cp bin/setup/setup.css $(DISTRIB_DIR)/gw/setup/
	cp bin/setup/lang/*.htm $(DISTRIB_DIR)/gw/setup/lang/
	cp bin/setup/lang/lexicon.txt $(DISTRIB_DIR)/gw/setup/lang/
	cp bin/setup/lang/intro.txt $(DISTRIB_DIR)/gw/setup/lang/
	cp -R hd/* $(DISTRIB_DIR)/gw/
	$(RM) $(DISTRIB_DIR)/exe-version.txt
	echo "Commit: `git log  -1 | grep commit | cut -c8-15`, `date`"      > $(DISTRIB_DIR)/commit.txt
	echo "`ocaml  -version`"      >> $(DISTRIB_DIR)/commit.txt
	echo "`camlp5 -v 2>&1`"       >> $(DISTRIB_DIR)/commit.txt
	echo "-----"                  >> $(DISTRIB_DIR)/commit.txt

.PHONY: install uninstall distrib

###### [END] Installation / Distribution section

doc: | piqi $(GENERATED_FILES_DEP)
	dune build @doc
.PHONY: doc

test: | piqi $(GENERATED_FILES_DEP)
	dune build @runtest
.PHONY: test

bench: | piqi $(GENERATED_FILES_DEP)
	dune build @runbench
.PHONY: bench

BENCH_FILE ?= /tmp/geneweb-bench.bin

bench-marshal: | piqi $(GENERATED_FILES_DEP) benchmark/bench.exe
ifdef BENCH_NAME
	_build/default/benchmark/bench.exe --marshal --name ${BENCH_NAME} ${BENCH_FILE}
else
	 $(error BENCH_NAME variable is empty)
endif
.PHONY: bench-marshal

bench-tabulate: | piqi $(GENERATED_FILES_DEP) benchmark/bench.exe
	_build/default/benchmark/bench.exe --tabulate ${BENCH_FILE}
.PHONY: bench-tabulate

clean:
	@echo -n "Cleaning..."
	@$(RM) $(GENERATED_FILES_DEP) lib/*_piqi*.ml
	@$(RM) -r $(DISTRIB_DIR)
	@dune clean
	@echo " Done!"
.PHONY: clean


ci:
	@ocaml ./configure.ml && BENCH_NAME=vanilla $(MAKE) -s clean test bench-marshal clean
	@ocaml ./configure.ml --sosa-num && BENCH_NAME=num $(MAKE) -s clean test bench-marshal clean
	@ocaml ./configure.ml --sosa-zarith && BENCH_NAME=zarith $(MAKE) -s clean test bench-marshal clean
ifndef OS
	@ocaml ./configure.ml --api && BENCH_NAME=api $(MAKE) -s clean bench-marshal test clean
endif
	@$(MAKE) -s bench-tabulate
.PHONY: ci
