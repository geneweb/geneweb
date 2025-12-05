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

COMPIL_DATE := $(shell date +'%Y-%m-%d')
COMMIT_DATE := $(shell git show -s --date=short --pretty=format:'%cd')
COMMIT_ID := $(shell git rev-parse --short HEAD)
COMMIT_TITLE := $(shell git log -1 --pretty="%s" | sed "s/\"/\\\"/g")
COMMIT_COMMENT:= $(shell git log -1 --pretty="%b" | sed "s/\"/\\\"/g")
BRANCH := $(shell git symbolic-ref --quiet --short HEAD || git branch -r --contains HEAD | head -n1 | tr -d ' ')
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

BUILD = dune build @bin/all @lib/all

info:
	@printf 'Building \033[1;1mGeneweb $(VERSION)\033[0m with $(OCAMLV).\n\n'
	@printf 'Repository \033[1;1m$(SOURCE)\033[0m. Branch \033[1;1m$(BRANCH)\033[0m. '
	@printf 'Last commit \033[1;1m$(COMMIT_ID)\033[0m message:\n\n'
	@printf '  \033[1;1m%s\033[0m\n' '$(subst ','\'',$(COMMIT_TITLE))'
ifneq ($(COMMIT_COMMENT),)
	@printf '\n$(subst ','\'',$(COMMIT_COMMENT))' | fmt -w 80
endif
	@printf '\n\033[1;1mGenerating configuration files\033[0m\n'
.PHONY: info

generated: lib/version.ml
	@printf "Done.\n"

fmt build build-geneweb gwd install uninstall: info generated

fmt: ## Format Ocaml code
	@printf "\n\033[1;1mOcamlformat\033[0m\n"
	dune build @fmt --auto-promote

# [BEGIN] Installation / Distribution section
build:
	dune build

build-geneweb: ## Build the geneweb package (libraries and binaries)
	@printf "\n\033[1;1mBuilding executables\033[0m\n"
	dune build @bin/all @lib/all
	@printf "Done."

build-geneweb-rpc: ## Build the geneweb-rpc package
	dune build @rpc/all

gwd: ## Build ondy gwd/gwc executables
	@printf "\n\033[1;1mBuilding only gwd and gwc executables\033[0m\n"
	dune build bin/gwd bin/gwc
	@printf "Done."

install: ## Install geneweb using dune
	dune build @install
	dune install

uninstall: ## Uninstall geneweb using dune
	dune build @install
	dune uninstall

distrib: info ## Build the project and copy what is necessary for distribution
	@$(MAKE) --no-print-directory generated
	@printf "\n\033[1;1mBuilding executables\n\033[0m"
	@$(BUILD)
	@printf "Done.\n"
	@$(RM) -r $(DISTRIB_DIR)
	@printf "\n\033[1;1mCreating distribution directory\033[0m\n"
	mkdir $(DISTRIB_DIR)
	mkdir -p $(DISTRIB_DIR)/bases
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
	cp -f etc/ROBOT.txt $(DISTRIB_DIR)/ROBOT.txt
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
	cp $(BUILD_DISTRIB_DIR)robot/robot.exe $(DISTRIB_DIR)/gw/robot$(EXT)
	cp $(BUILD_DISTRIB_DIR)setup/setup.exe $(DISTRIB_DIR)/gw/gwsetup$(EXT)
	cp $(BUILD_DISTRIB_DIR)update_nldb/update_nldb.exe $(DISTRIB_DIR)/gw/update_nldb$(EXT)
	@printf "\n\033[1;1m└ Copy templates in $(DISTRIB_DIR)/gw/\033[0m\n"
	cp -R hd/* $(DISTRIB_DIR)/gw/
	rm $(DISTRIB_DIR)/gw/etc/js/checkdata.js
	rm $(DISTRIB_DIR)/gw/etc/js/p_mod.js
	rm $(DISTRIB_DIR)/gw/etc/js/relationmatrix.js
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
doc:
	dune build @doc
.PHONY: doc

opendoc: doc
	xdg-open $(ODOC_DIR)/index.html
.PHONY: opendoc

test: ## Run tests
test:
	@dune build @runtest
.PHONY: test

bench: ## Run benchmarks
bench:
	dune build @runbench
.PHONY: bench

BENCH_FILE?=geneweb-bench.bin

bench-marshal: ## Run benchmarks and record the result
bench-marshal:
ifdef BENCH_NAME
	dune exec benchmark/bench.exe -- --marshal --name ${BENCH_NAME} ${BENCH_FILE}
else
	 $(error BENCH_NAME variable is empty)
endif
.PHONY: bench-marshal

bench-tabulate: ## Read BENCH_FILE and print a report
bench-tabulate:
	dune exec benchmark/bench.exe -- --tabulate ${BENCH_FILE}
	@$(RM) $(BENCH_FILE)
.PHONY: bench-tabulate

clean:
	@echo -n "Cleaning…"
	@$(RM) -r $(DISTRIB_DIR)
	@dune clean
	@echo " Done."
.PHONY: clean

ci: ## Run tests, skip known failures
ci:
	@GENEWEB_CI=on dune build @runtest

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
