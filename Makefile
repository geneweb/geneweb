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
DISTRIB_DIR=distribution
BUILD_DIR=_build/default
BUILD_DISTRIB_DIR=$(BUILD_DIR)/bin/
ODOC_DIR=$(BUILD_DIR)/_doc/_html

# [BEGIN] Generated files section

CPPO_D=$(OS_D) $(SYSLOG_D)

%/dune: %/dune.in Makefile.config
	@printf "Generating $@…" \
	&& cat $< \
	| cppo -n $(CPPO_D) \
	| sed \
	-e 's/%%%CPPO_D%%%/$(CPPO_D)/g' \
	-e 's/%%%SYSLOG_PKG%%%/$(SYSLOG_PKG)/g' \
	-e 's/%%%DUNE_DIRS_EXCLUDE%%%/$(DUNE_DIRS_EXCLUDE)/g' \
	-e 's/%%%ANCIENT_LIB%%%/$(ANCIENT_LIB)/g' \
	-e 's/%%%ANCIENT_FILE%%%/$(ANCIENT_FILE)/g' \
	> $@ \
	&& printf " Done.\n"

COMPIL_DATE := $(shell date +'%Y-%m-%d')
COMMIT_DATE := $(shell git show -s --date=short --pretty=format:'%cd')
COMMIT_ID := $(shell git rev-parse --short HEAD)
COMMIT_TITLE := $(shell git log -1 --pretty="%s" | sed "s/\"/\\\"/g")
COMMIT_COMMENT:= $(shell git log -1 --pretty="%b" | sed "s/\"/\\\"/g")
BRANCH := $(shell git rev-parse --abbrev-ref HEAD)
VERSION := $(shell awk -F\" '/er =/ {print $$2}' lib/version.txt)
SOURCE := $(shell git remote get-url origin | sed -n 's|^.*github.com.\([^/]\+/[^/.]\+\)\(.git\)\?|\1|p')
OCAMLV := $(shell ocaml --version)

lib/version.ml:
	@cp lib/version.txt $@
	@printf 'let branch = "$(BRANCH)"\n' >> $@
	@printf 'let src = "$(SOURCE)"\n' >> $@
	@printf 'let commit_id = "$(COMMIT_ID)"\n' >> $@
	@printf 'let commit_date = "$(COMMIT_DATE)"\n' >> $@
	@printf 'let compil_date = "$(COMPIL_DATE)"\n' >> $@
	@printf 'Generating $@… Done.\n'
.PHONY: install lib/version.ml

# Patch/unpatch files for campl5 >= 8.03
CAMLP5_VERSION := $(shell camlp5 -version 2>/dev/null || echo 0)
CAMLP5_MAJOR := $(shell echo $(CAMLP5_VERSION) | cut -d '.' -f 1)
CAMLP5_MINOR := $(shell echo $(CAMLP5_VERSION) | cut -d '.' -f 2)

patch_files:
	@if [ '$(CAMLP5_VERSION)' != 0 ] && [ $(CAMLP5_MAJOR) -eq 8 ] && [ $(CAMLP5_MINOR) -ge 3 ]; then \
	  printf "\nPatching bin/ged2gwb/dune.in and ged2gwb.ml for camlp5 version $(CAMLP5_VERSION) (>= 8.03.00)… Done.\n"; \
	  perl -pi.bak -e 's|\(preprocess \(action \(run camlp5o pr_o.cmo pa_extend.cmo q_MLast.cmo %\{input-file\}\)\)\)|\(preprocess \(action \(run not-ocamlfind preprocess -package camlp5.extend,camlp5.quotations,camlp5.pr_o -syntax camlp5o %\{input-file\}\)\)\)|' bin/ged2gwb/dune.in; \
	  if [ "$(OS_TYPE)" = "Win" ]; then \
	    perl -0777 -pi.bak -e 's/(; Token\.tok_comm = None)(\s*\})/\1\r\n  ; Token.kwds = Hashtbl.create 301\2/' bin/ged2gwb/ged2gwb.ml; \
	  else \
	    perl -0777 -pi.bak -e 's/(; Token\.tok_comm = None)(\n  \})/$$1\n  ; Token.kwds = Hashtbl.create 301$$2/' bin/ged2gwb/ged2gwb.ml; \
	  fi \
	fi

unpatch_files:
	@if [ -f bin/ged2gwb/dune.in.bak ] && [ -f bin/ged2gwb/ged2gwb.ml.bak ]; then \
	  printf "Restoring original patched files… Done.\n"; \
	  mv bin/ged2gwb/dune.in.bak bin/ged2gwb/dune.in; \
	  mv bin/ged2gwb/ged2gwb.ml.bak bin/ged2gwb/ged2gwb.ml; \
	fi

BUILD = dune build @bin/all @lib/all
UNPATCH = $(MAKE) --no-print-directory unpatch_files

unpatch_after = (($(1) && $(UNPATCH)) || ($(UNPATCH) && false))

info:
	@printf 'Building \033[1;1mGeneweb $(VERSION)\033[0m with $(OCAMLV).\n\n'
	@printf 'Repository \033[1;1m$(SOURCE)\033[0m. Branch \033[1;1m$(BRANCH)\033[0m. '
	@printf 'Last commit \033[1;1m$(COMMIT_ID)\033[0m message:\n\n'
	@printf '  \033[1;1m%s\033[0m\n' '$(subst ','\'',$(COMMIT_TITLE))'
ifneq ($(COMMIT_COMMENT),)
	@printf '\n$(subst ','\'',$(COMMIT_COMMENT))' | fmt -w 80
endif
	@printf '\n\033[1;1mGenerating configuration files\033[0m\n'
.PHONY: patch_files unpatch_files info

DUNE_IN_FILES := $(shell find lib bin test -name "dune.in")
DUNE_FILES := $(patsubst %.in,%,$(DUNE_IN_FILES))

generated: $(DUNE_FILES) lib/version.ml
	@printf "Done.\n"

fmt build build-geneweb gwd install uninstall: info patch_files generated

fmt: ## Format Ocaml code
	@printf "\n\033[1;1mOcamlformat\033[0m\n"
	$(call unpatch_after, dune build @fmt --auto-promote)

# [BEGIN] Installation / Distribution section
build:
	@$(call unpatch_after, dune build)

build-geneweb: ## Build the geneweb package (libraries and binaries)
	@printf "\n\033[1;1mBuilding executables\033[0m\n"
	@$(call unpatch_after, dune build @bin/all @lib/all)
	@printf "Done."

build-geneweb-rpc: ## Build the geneweb-rpc package
	dune build @rpc/all

gwd: ## Build ondy gwd/gwc executables
	@printf "\n\033[1;1mBuilding only gwd and gwc executables\033[0m\n"
	@$(call unpatch_after, dune build bin/gwd bin/gwc)
	@printf "Done."

install: ## Install geneweb using dune
	$(call unpatch_after, dune build @install)
	dune install

uninstall: ## Uninstall geneweb using dune
	$(call unpatch_after, dune build @install)
	dune uninstall

distrib: info ## Build the project and copy what is necessary for distribution
	@$(MAKE) --no-print-directory patch_files generated
	@printf "\n\033[1;1mBuilding executables\n\033[0m"
	@$(call unpatch_after, $(BUILD))
	@printf "Done.\n"
	@$(RM) -r $(DISTRIB_DIR)
	@printf "\n\033[1;1mCreating distribution directory\033[0m\n"
	mkdir $(DISTRIB_DIR)
	mkdir -p $(DISTRIB_DIR)/bases
	mkdir -p $(DISTRIB_DIR)/bases/etc
	mkdir -p $(DISTRIB_DIR)/bases/lang
	mkdir -p $(DISTRIB_DIR)/bases/src
	cp CHANGES $(DISTRIB_DIR)/CHANGES.txt
	cp LICENSE $(DISTRIB_DIR)/LICENSE.txt
	cp etc/README.txt $(DISTRIB_DIR)/.
	cp etc/LISEZMOI.txt $(DISTRIB_DIR)/.
	cp etc/START.htm $(DISTRIB_DIR)/.
ifeq ($(OS_TYPE),Win)
	cp etc/Windows/gwd.bat $(DISTRIB_DIR)
	cp etc/Windows/gwsetup.bat $(DISTRIB_DIR)
	cp -f etc/Windows/README.txt $(DISTRIB_DIR)/README.txt
	cp -f etc/Windows/LISEZMOI.txt $(DISTRIB_DIR)/LISEZMOI.txt
else ifeq ($(OS_TYPE),Darwin)
	cp etc/gwd.sh $(DISTRIB_DIR)
	cp etc/gwsetup.sh $(DISTRIB_DIR)
	cp etc/macOS/geneweb.sh $(DISTRIB_DIR)
else
	cp etc/gwd.sh $(DISTRIB_DIR)/gwd.sh
	cp etc/gwsetup.sh $(DISTRIB_DIR)/gwsetup.sh
endif
	mkdir $(DISTRIB_DIR)/gw
	cp etc/a.gwf $(DISTRIB_DIR)/gw/.
	echo "-setup_link" > $(DISTRIB_DIR)/gw/gwd.arg
	@printf "\n\033[1;1m└ Copy binaries in $(DISTRIB_DIR)/gw/\033[0m\n"
	cp $(BUILD_DISTRIB_DIR)connex/connex.exe $(DISTRIB_DIR)/gw/connex$(EXT)
	cp $(BUILD_DISTRIB_DIR)consang/consang.exe $(DISTRIB_DIR)/gw/consang$(EXT)
	cp $(BUILD_DISTRIB_DIR)fixbase/gwfixbase.exe $(DISTRIB_DIR)/gw/gwfixbase$(EXT)
	cp $(BUILD_DISTRIB_DIR)ged2gwb/ged2gwb.exe $(DISTRIB_DIR)/gw/ged2gwb$(EXT)
	cp $(BUILD_DISTRIB_DIR)gwb2ged/gwb2ged.exe $(DISTRIB_DIR)/gw/gwb2ged$(EXT)
	cp $(BUILD_DISTRIB_DIR)cache_files/cache_files.exe $(DISTRIB_DIR)/gw/cache_files$(EXT)
	cp $(BUILD_DISTRIB_DIR)gwc/gwc.exe $(DISTRIB_DIR)/gw/gwc$(EXT)
	cp $(BUILD_DISTRIB_DIR)gwd/gwd.exe $(DISTRIB_DIR)/gw/gwd$(EXT)
	cp $(BUILD_DISTRIB_DIR)gwdiff/gwdiff.exe $(DISTRIB_DIR)/gw/gwdiff$(EXT)
	cp $(BUILD_DISTRIB_DIR)gwu/gwu.exe $(DISTRIB_DIR)/gw/gwu$(EXT)
	cp $(BUILD_DISTRIB_DIR)setup/setup.exe $(DISTRIB_DIR)/gw/gwsetup$(EXT)
	cp $(BUILD_DISTRIB_DIR)update_nldb/update_nldb.exe $(DISTRIB_DIR)/gw/update_nldb$(EXT)
	@printf "\n\033[1;1m└ Copy templates in $(DISTRIB_DIR)/gw/\033[0m\n"
	cp -R hd/* $(DISTRIB_DIR)/gw/
	mkdir $(DISTRIB_DIR)/gw/setup
	cp bin/setup/intro.txt $(DISTRIB_DIR)/gw/setup/
	mkdir $(DISTRIB_DIR)/gw/setup/lang
	cp bin/setup/setup.gwf $(DISTRIB_DIR)/gw/setup/
	cp bin/setup/setup.css $(DISTRIB_DIR)/gw/setup/
	cp bin/setup/lang/*.htm $(DISTRIB_DIR)/gw/setup/lang/
	cp bin/setup/lang/lexicon.txt $(DISTRIB_DIR)/gw/setup/lang/
	cp bin/setup/lang/intro.txt $(DISTRIB_DIR)/gw/setup/lang/
	@printf "\n\033[1;1m└ Copy plugins in $(DISTRIB_DIR)/gw/plugins\033[0m\n"
	mkdir $(DISTRIB_DIR)/gw/plugins
	@for P in $(shell ls plugins); do \
	  if [ -f $(BUILD_DIR)/plugins/$$P/plugin_$$P.cmxs ] ; then \
	    mkdir $(DISTRIB_DIR)/gw/plugins/$$P; \
	    printf "cp %s %s\n" "$(BUILD_DIR)/plugins/$$P/plugin_$$P.cmxs" "$(DISTRIB_DIR)/gw/plugins/$$P/"; \
	    cp $(BUILD_DIR)/plugins/$$P/plugin_$$P.cmxs $(DISTRIB_DIR)/gw/plugins/$$P/; \
	    if [ -d plugins/$$P/assets ] ; then \
	      printf "cp -R %s %s\n" "$(BUILD_DIR)/plugins/$$P/assets" "$(DISTRIB_DIR)/gw/plugins/$$P/"; \
	      cp -R $(BUILD_DIR)/plugins/$$P/assets $(DISTRIB_DIR)/gw/plugins/$$P/; \
	    fi; \
	    if [ -f $(BUILD_DIR)/plugins/$$P/META ] ; then \
	      printf "cp %s %s\n" "$(BUILD_DIR)/plugins/$$P/META" "$(DISTRIB_DIR)/gw/plugins/$$P/"; \
	      cp $(BUILD_DIR)/plugins/$$P/META $(DISTRIB_DIR)/gw/plugins/$$P/; \
	    fi; \
	  fi; \
	done
	@printf "Done.\n\n\033[1;1mDistribution complete\033[0m\n"
	@printf "You can launch Geneweb with “\033[1;1mcd $(DISTRIB_DIR)\033[0m” followed by “\033[1;1mgw/gwd$(EXT)\033[0m”.\n"

.PHONY: build build-geneweb build-geneweb-rpc gwd fmt install uninstall distrib

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
	@$(call unpatch_after, dune build @runtest)
.PHONY: test

bench: ## Run benchmarks
bench: | $(GENERATED_FILES_DEP)
	dune build @runbench
.PHONY: bench

BENCH_FILE?=geneweb-bench.bin

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
	@$(RM) $(BENCH_FILE)
.PHONY: bench-tabulate

clean:
	@echo -n "Cleaning…"
	@$(RM) $(GENERATED_FILES_DEP)
	@$(RM) -r $(DISTRIB_DIR)
	@dune clean
	@echo " Done."
.PHONY: clean

ci: ## Run tests, skip known failures
ci: | $(GENERATED_FILES_DEP)
	@$(call unpatch_after, GENEWEB_CI=on dune build @runtest)

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
