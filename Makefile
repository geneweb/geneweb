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
RESOURCES_DIR=../geneweb-resources
DISTRIB_DIR=distribution
BUILD_DIR=_build/default
ODOC_DIR=$(BUILD_DIR)/_doc/_html

# [BEGIN] Generated files section

lib/gwlib.ml:
	@echo -n "Generating $@..."
	@echo -n "let prefix = Option.value (Sys.getenv_opt \"GWPREFIX\")" > $@
	@echo " ~default:\"$(PREFIX)\"" | sed -e 's|\\|/|g' >> $@
	@echo " Done!"

lib/dev_config.ml:
	@echo -n "Generating $@..."
	@echo -n "let debug = " > $@
ifeq ($(DUNE_PROFILE),dev)
	@echo "true" >> $@
else
	@echo "false" >> $@
endif
	@echo " Done!"

%/dune: %/dune.in Makefile.config
	@echo -n "Generating $@..." \
	&& cat $< \
	| sed \
	-e "s/%%%GWDB_PKG%%%/$(GWDB_PKG)/g" \
	-e "s/%%%SYSLOG_PKG%%%/$(SYSLOG_PKG)/g" \
	> $@ \
	&& echo " Done!"

dune-workspace: dune-workspace.in Makefile.config
	cat $< | sed  -e "s/%%%DUNE_PROFILE%%%/$(DUNE_PROFILE)/g" > $@

# [End] Generated files section

GENERATED_FILES_DEP = \
	dune-workspace \
	test/dune \
	lib/gwlib.ml \
	lib/dev_config.ml \
	lib/util/dune \
	bin/consang/dune \
	bin/fixbase/dune \
	bin/ged2gwb/dune \
	bin/gwb2ged/dune \
	bin/gwc/dune \
	bin/gwd/dune \
	bin/populate-xss-gwb/dune \
	bin/gwu/dune \
	bin/setup/dune \
	bin/update_nldb/dune \
	bin/init-search-indexes/dune

generated: $(GENERATED_FILES_DEP)

install uninstall build distrib: $(GENERATED_FILES_DEP)

fmt:
	dune build @fmt --auto-promote

# [BEGIN] Installation / Distribution section

build: ## Build the geneweb package (librairies and binaries)
build:
	-dune build @fmt --auto-promote
	dune build -p geneweb --profile $(DUNE_PROFILE)

install: ## Install geneweb using dune
install:
	dune build @install --profile $(DUNE_PROFILE)
	dune install

uninstall: ## Uninstall geneweb using dune
uninstall:
	dune build @install --profile $(DUNE_PROFILE)
	dune uninstall

BUILD_DISTRIB_DIR=$(BUILD_DIR)/bin/

distrib: ## Build the project and copy what is necessary for distribution
distrib:
	dune build -p geneweb --profile $(DUNE_PROFILE)
	$(RM) -r $(DISTRIB_DIR)
	mkdir $(DISTRIB_DIR)
	mkdir -p $(DISTRIB_DIR)/bases
	cp CHANGES $(DISTRIB_DIR)/CHANGES.txt
	cp LICENSE $(DISTRIB_DIR)/LICENSE.txt
	cp etc/README.txt $(DISTRIB_DIR)/.
	cp etc/LISEZMOI.txt $(DISTRIB_DIR)/.
	cp etc/START.htm $(DISTRIB_DIR)/.
	cp -R etc/install-cgi $(DISTRIB_DIR)
	cp etc/gwd.sh $(DISTRIB_DIR)/gwd.sh
	cp etc/gwsetup.sh $(DISTRIB_DIR)/gwsetup.sh
	mkdir $(DISTRIB_DIR)/gw
	cp etc/a.gwf $(DISTRIB_DIR)/gw/.
	echo "-setup_link" > $(DISTRIB_DIR)/gw/gwd.arg
	cp $(BUILD_DISTRIB_DIR)consang/consang.exe $(DISTRIB_DIR)/gw/consang;
	cp $(BUILD_DISTRIB_DIR)fixbase/gwfixbase.exe $(DISTRIB_DIR)/gw/gwfixbase;
	cp $(BUILD_DISTRIB_DIR)ged2gwb/ged2gwb.exe $(DISTRIB_DIR)/gw/ged2gwb;
	cp $(BUILD_DISTRIB_DIR)gwb2ged/gwb2ged.exe $(DISTRIB_DIR)/gw/gwb2ged;
	cp $(BUILD_DISTRIB_DIR)gwc/gwc.exe $(DISTRIB_DIR)/gw/gwc;
	cp $(BUILD_DISTRIB_DIR)gwd/gwd.exe $(DISTRIB_DIR)/gw/gwd;
	cp $(BUILD_DISTRIB_DIR)populate-xss-gwb/populate_xss_gwb.exe $(DISTRIB_DIR)/gw/populate-xss-gwb;
	cp $(BUILD_DISTRIB_DIR)gwu/gwu.exe $(DISTRIB_DIR)/gw/gwu;
	cp $(BUILD_DISTRIB_DIR)setup/setup.exe $(DISTRIB_DIR)/gw/gwsetup;
	cp $(BUILD_DISTRIB_DIR)update_nldb/update_nldb.exe $(DISTRIB_DIR)/gw/update_nldb;
	cp $(BUILD_DISTRIB_DIR)init-search-indexes/init_search_indexes.exe $(DISTRIB_DIR)/gw/init-search-indexes;
	mkdir $(DISTRIB_DIR)/gw/setup
	cp bin/setup/intro.txt $(DISTRIB_DIR)/gw/setup/
	mkdir $(DISTRIB_DIR)/gw/setup/lang
	cp bin/setup/setup.gwf $(DISTRIB_DIR)/gw/setup/
	cp bin/setup/setup.css $(DISTRIB_DIR)/gw/setup/
	cp bin/setup/lang/*.htm $(DISTRIB_DIR)/gw/setup/lang/
	cp bin/setup/lang/lexicon.txt $(DISTRIB_DIR)/gw/setup/lang/
	cp bin/setup/lang/intro.txt $(DISTRIB_DIR)/gw/setup/lang/
	cp -R $(RESOURCES_DIR)/etc/ $(DISTRIB_DIR)/gw/
	cp -R $(RESOURCES_DIR)/images/ $(DISTRIB_DIR)/gw/
	cp -R $(RESOURCES_DIR)/lang/ $(DISTRIB_DIR)/gw/
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
	ocaml ./configure.ml --release
	dune build @runtest
.PHONY: test

clean:
	@echo -n "Cleaning..."
	@$(RM) $(GENERATED_FILES_DEP) lib/*_piqi*.ml
	@$(RM) -r $(DISTRIB_DIR)
	@dune clean
	@echo " Done!"
.PHONY: clean

ci: ## Run unit tests
ci:
	@ocaml ./configure.ml --gwdb-test --release && $(MAKE) -s clean build && GENEWEB_CI=on dune runtest
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
