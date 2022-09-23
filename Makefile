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
ODOC_DIR=$(BUILD_DIR)/_doc/_html

# [BEGIN] Generated files section

lib/gwlib.ml:
	@echo -n "Generating $@..."
	@echo "let prefix =" > $@
	@echo "  try Sys.getenv \"GWPREFIX\"" >> $@
	@echo "  with Not_found -> \"$(PREFIX)\"" | sed -e 's|\\|/|g' >> $@
	@echo " Done!"

CPPO_D=$(GWDB_D) $(OS_D) $(SYSLOG_D) $(SOSA_D)

ifeq ($(DUNE_PROFILE),dev)
    CPPO_D+= -D DEBUG
endif

%/dune: %/dune.in Makefile.config
	@echo -n "Generating $@..." \
	&& cat $< \
	| cppo -n $(CPPO_D) \
	| sed \
	-e "s/%%%CPPO_D%%%/$(CPPO_D)/g" \
	-e "s/%%%SOSA_PKG%%%/$(SOSA_PKG)/g" \
	-e "s/%%%GWDB_PKG%%%/$(GWDB_PKG)/g" \
	-e "s/%%%SYSLOG_PKG%%%/$(SYSLOG_PKG)/g" \
	-e "s/%%%DUNE_DIRS_EXCLUDE%%%/$(DUNE_DIRS_EXCLUDE)/g" \
	> $@ \
	&& echo " Done!"

bin/gwrepl/.depend:
	@echo -n "Generating $@..."
	@pwd > $@
	@dune top bin/gwrepl >> $@
	@echo " Done!"

hd/etc/version.txt:
	@echo -n "Generating $@..."
	@echo "GeneWeb[:] [compiled on %s from commit %s:::" > $@
	@echo "$$(date '+%Y-%m-%d'):" >> $@
	@echo "$$(git show -s --date=short --pretty=format:'<a href="https://github.com/geneweb/geneweb/commit/%h">%h (%cd)</a>')]" >> $@
	@echo " Done!"
.PHONY:hd/etc/version.txt

# [End] Generated files section

GENERATED_FILES_DEP = \
	hd/etc/version.txt \
	lib/dune \
	lib/gwdb/dune \
	lib/core/dune \
	lib/gwlib.ml \
	lib/util/dune \
	benchmark/dune \
	bin/connex/dune \
	bin/consang/dune \
	bin/fixbase/dune \
	bin/ged2gwb/dune \
	bin/gwb2ged/dune \
	bin/gwc/dune \
	bin/gwd/dune \
	bin/gwdiff/dune \
	bin/gwgc/dune \
	bin/gwrepl/dune \
	bin/gwrepl/.depend \
	bin/gwu/dune \
	bin/setup/dune \
	bin/update_nldb/dune \
	test/dune \

generated: $(GENERATED_FILES_DEP)

install uninstall build distrib: $(GENERATED_FILES_DEP)

# [BEGIN] Installation / Distribution section

build: ## Build the geneweb package (librairies and binaries)
build:
	dune build -p geneweb --profile $(DUNE_PROFILE)

install: ## Install geneweb using dune
install:
	dune build @install
	dune install

uninstall: ## Uninstall geneweb using dune
uninstall:
	dune build @install
	dune uninstall

BUILD_DISTRIB_DIR=$(BUILD_DIR)/bin/

distrib: ## Build the project and copy what is necessary for distribution
distrib:
	dune build -p geneweb
	$(RM) -r $(DISTRIB_DIR)
	mkdir $(DISTRIB_DIR)
	mkdir -p $(DISTRIB_DIR)/bases
	cp CHANGES $(DISTRIB_DIR)/CHANGES.txt
	cp LICENSE $(DISTRIB_DIR)/LICENSE.txt
	cp etc/README.txt $(DISTRIB_DIR)/.
	cp etc/LISEZMOI.txt $(DISTRIB_DIR)/.
	cp etc/START.htm $(DISTRIB_DIR)/.
	cp -R etc/install-cgi $(DISTRIB_DIR)
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
	echo "-setup_link" > $(DISTRIB_DIR)/gw/gwd.arg
	cp $(BUILD_DISTRIB_DIR)connex/connex.exe $(DISTRIB_DIR)/gw/connex$(EXT);
	cp $(BUILD_DISTRIB_DIR)consang/consang.exe $(DISTRIB_DIR)/gw/consang$(EXT);
	cp $(BUILD_DISTRIB_DIR)fixbase/gwfixbase.exe $(DISTRIB_DIR)/gw/gwfixbase$(EXT);
	cp $(BUILD_DISTRIB_DIR)ged2gwb/ged2gwb.exe $(DISTRIB_DIR)/gw/ged2gwb$(EXT);
	cp $(BUILD_DISTRIB_DIR)gwb2ged/gwb2ged.exe $(DISTRIB_DIR)/gw/gwb2ged$(EXT);
	cp $(BUILD_DISTRIB_DIR)gwc/gwc.exe $(DISTRIB_DIR)/gw/gwc$(EXT);
	cp $(BUILD_DISTRIB_DIR)gwd/gwd.exe $(DISTRIB_DIR)/gw/gwd$(EXT);
	cp $(BUILD_DISTRIB_DIR)gwdiff/gwdiff.exe $(DISTRIB_DIR)/gw/gwdiff$(EXT);
	if test -f $(BUILD_DISTRIB_DIR)gwrepl/gwrepl.bc ; then cp $(BUILD_DISTRIB_DIR)gwrepl/gwrepl.bc $(DISTRIB_DIR)/gw/gwrepl$(EXT); fi
	cp $(BUILD_DISTRIB_DIR)gwu/gwu.exe $(DISTRIB_DIR)/gw/gwu$(EXT);
	cp $(BUILD_DISTRIB_DIR)setup/setup.exe $(DISTRIB_DIR)/gw/gwsetup$(EXT);
	cp $(BUILD_DISTRIB_DIR)update_nldb/update_nldb.exe $(DISTRIB_DIR)/gw/update_nldb$(EXT);
	mkdir $(DISTRIB_DIR)/gw/setup
	cp bin/setup/intro.txt $(DISTRIB_DIR)/gw/setup/
	mkdir $(DISTRIB_DIR)/gw/setup/lang
	cp bin/setup/setup.gwf $(DISTRIB_DIR)/gw/setup/
	cp bin/setup/setup.css $(DISTRIB_DIR)/gw/setup/
	cp bin/setup/lang/*.htm $(DISTRIB_DIR)/gw/setup/lang/
	cp bin/setup/lang/lexicon.txt $(DISTRIB_DIR)/gw/setup/lang/
	cp bin/setup/lang/intro.txt $(DISTRIB_DIR)/gw/setup/lang/
	cp -R hd/* $(DISTRIB_DIR)/gw/
	mkdir $(DISTRIB_DIR)/gw/plugins
	for P in $(shell ls plugins); do \
		if [ -f $(BUILD_DIR)/plugins/$$P/plugin_$$P.cmxs ] ; then \
			mkdir $(DISTRIB_DIR)/gw/plugins/$$P; \
			cp $(BUILD_DIR)/plugins/$$P/plugin_$$P.cmxs $(DISTRIB_DIR)/gw/plugins/$$P/; \
			if [ -d plugins/$$P/assets ] ; then \
				cp -R $(BUILD_DIR)/plugins/$$P/assets $(DISTRIB_DIR)/gw/plugins/$$P/; \
			fi; \
			if [ -f $(BUILD_DIR)/plugins/$$P/META ] ; then \
				cp $(BUILD_DIR)/plugins/$$P/META $(DISTRIB_DIR)/gw/plugins/$$P/; \
			fi; \
		fi; \
	done

.PHONY: install uninstall distrib

# [END] Installation / Distribution section

doc: ## Documentation generation
doc: | $(GENERATED_FILES_DEP)
	dune build @doc
.PHONY: doc

opendoc: doc
	xdg-open $(ODOC_DIR)/index.html
.PHONY: opendoc

test: ## Run tests
test: | $(GENERATED_FILES_DEP)
	dune build @runtest
.PHONY: test

bench: ## Run benchmarks
bench: | $(GENERATED_FILES_DEP)
	dune build @runbench
.PHONY: bench

BENCH_FILE ?= /tmp/geneweb-bench.bin

bench-marshal: ## Run benchmarks and record the result
bench-marshal: | $(GENERATED_FILES_DEP)
ifdef BENCH_NAME
	dune exec benchmark/bench.exe -- --marshal --name ${BENCH_NAME} ${BENCH_FILE}
else
	 $(error BENCH_NAME variable is empty)
endif
.PHONY: bench-marshal

bench-tabulate: ## Read BENCH_FILE and print a report
bench-tabulate: | $(GENERATED_FILES_DEP)
	dune exec benchmark/bench.exe -- --tabulate ${BENCH_FILE}
.PHONY: bench-tabulate

clean:
	@echo -n "Cleaning..."
	@$(RM) $(GENERATED_FILES_DEP) lib/*_piqi*.ml
	@$(RM) -r $(DISTRIB_DIR)
	@dune clean
	@echo " Done!"
.PHONY: clean

ci: ## Run unit tests and benchmark with different configurations
ci:
	@ocaml ./configure.ml && BENCH_NAME=vanilla $(MAKE) -s clean test bench-marshal clean
	@ocaml ./configure.ml --sosa-num && BENCH_NAME=num $(MAKE) -s clean test bench-marshal clean
	@ocaml ./configure.ml --sosa-zarith && BENCH_NAME=zarith $(MAKE) -s clean test bench-marshal clean
	@$(MAKE) -s bench-tabulate
.PHONY: ci

ocp-indent: ## Run ocp-indent (inplace edition)
ocp-indent:
	for f in `find lib bin -type f -regex .*[.]ml[i]?` ; do \
		echo $$f ; \
		ocp-indent -i $$f ; \
	done
.PHONY: ocp-indent

.DEFAULT_GOAL := help
help:
	@clear;grep -E '(^[a-zA-Z_-]+:.*?##.*$$)|(^##)' Makefile | awk 'BEGIN {FS = ":.*?#\
# "}; {printf "\033[32m%-30s\033[0m %s\n", $$1, $$2}' | sed -e 's/\[32m## /[33m/'
.PHONY: help
