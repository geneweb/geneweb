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

CPPO_D=$(GWDB_D) $(OS_D) $(SYSLOG_D) $(SOSA_D)

ifeq ($(DUNE_PROFILE),dev)
    CPPO_D+= -D DEBUG
endif

%/dune: %/dune.in Makefile.config
	@printf "Generating $@…" \
	&& cat $< \
	| cppo -n $(CPPO_D) \
	| sed \
	-e "s/%%%CPPO_D%%%/$(CPPO_D)/g" \
	-e "s/%%%SOSA_PKG%%%/$(SOSA_PKG)/g" \
	-e "s/%%%GWDB_PKG%%%/$(GWDB_PKG)/g" \
	-e "s/%%%SYSLOG_PKG%%%/$(SYSLOG_PKG)/g" \
	-e "s/%%%DUNE_DIRS_EXCLUDE%%%/$(DUNE_DIRS_EXCLUDE)/g" \
	-e "s/%%%ANCIENT_LIB%%%/$(ANCIENT_LIB)/g" \
	-e "s/%%%ANCIENT_FILE%%%/$(ANCIENT_FILE)/g" \
	> $@ \
	&& printf " Done.\n"

bin/gwrepl/.depend:
	@printf "Generating $@…"
	@pwd > $@
	@dune top bin/gwrepl >> $@
	@printf " Done.\n"

dune-workspace: dune-workspace.in Makefile.config
	@cat $< | sed  -e "s/%%%DUNE_PROFILE%%%/$(DUNE_PROFILE)/g" > $@

COMPIL_DATE := $(shell date +'%Y-%m-%d')
COMMIT_DATE := $(shell git show -s --date=short --pretty=format:'%cd')
COMMIT_ID := $(shell git rev-parse --short HEAD)
COMMIT_MSG := $(shell git log -1 --pretty="%s%n%n%b" | sed 's/"/\\"/g')
BRANCH := $(shell git rev-parse --abbrev-ref HEAD)
VERSION := $(shell awk -F\" '/er =/ {print $$2}' lib/version.txt)
SOURCE := $(shell git remote get-url origin | sed -n 's|^.*m/\([^/]\+/[^/.]\+\)\(.git\)\?|\1|p')
OCAMLV := $(shell ocaml --version)

lib/version.ml:
	@cp lib/version.txt $@
	@printf "let branch = \"$(BRANCH)\"\n" >> $@
	@printf "let src = \"$(SOURCE)\"\n" >> $@
	@printf "let commit_id = \"$(COMMIT_ID)\"\n" >> $@
	@printf "let commit_date = \"$(COMMIT_DATE)\"\n" >> $@
	@printf "let compil_date = \"$(COMPIL_DATE)\"\n" >> $@
	@printf "Generating $@… Done.\n"
.PHONY: lib/version.ml

info:
	@printf "Building \033[1;37mGeneweb $(VERSION)\033[0m with $(OCAMLV).\n\n"
	@printf "Repository \033[1;37m$(SOURCE)\033[0m. Branch \033[1;37m$(BRANCH)\033[0m.\n\n"
	@printf "Last commit \033[1;37m$(COMMIT_ID)\033[0m with message “\033[1;37m%s\033[0m”.\n" '$(subst ','\'',$(COMMIT_MSG))'
	@printf "\n\033[1;37mGenerating configuration files\033[0m\n"

GENERATED_FILES_DEP = \
	dune-workspace \
	lib/version.ml \
	lib/dune \
	lib/gwdb/dune \
	lib/core/dune \
	lib/util/dune \
	lib/ancient/dune \
	benchmark/dune \
	bin/connex/dune \
	bin/cache_files/dune \
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

install uninstall fmt build distrib: info $(GENERATED_FILES_DEP)

fmt: ## Format Ocaml code
ifneq ($(OS_TYPE),Win)
	@printf "\n\033[1;37mOcamlformat\033[0m\n"
	dune build @fmt --auto-promote
endif

# [BEGIN] Installation / Distribution section

build: ## Build the geneweb package (libraries and binaries)
build:
	@printf "\n\033[1;37mBuilding executables\033[0m\n"
	dune build -p geneweb --profile $(DUNE_PROFILE)

install: ## Install geneweb using dune
install:
	dune build @install --profile $(DUNE_PROFILE)
	dune install

uninstall: ## Uninstall geneweb using dune
uninstall:
	dune build @install --profile $(DUNE_PROFILE)
	dune uninstall

distrib: build ## Build the project and copy what is necessary for distribution
distrib:
	$(RM) -r $(DISTRIB_DIR)
	@printf "\n\033[1;37mCreating distribution directory\033[0m\n"
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
	@printf "\n\033[1;37m└ Copy binaries in $(DISTRIB_DIR)/gw/\033[0m\n"
	cp $(BUILD_DISTRIB_DIR)connex/connex.exe $(DISTRIB_DIR)/gw/connex$(EXT)
	cp $(BUILD_DISTRIB_DIR)consang/consang.exe $(DISTRIB_DIR)/gw/consang$(EXT)
	cp $(BUILD_DISTRIB_DIR)fixbase/gwfixbase.exe $(DISTRIB_DIR)/gw/gwfixbase$(EXT)
	cp $(BUILD_DISTRIB_DIR)ged2gwb/ged2gwb.exe $(DISTRIB_DIR)/gw/ged2gwb$(EXT)
	cp $(BUILD_DISTRIB_DIR)gwb2ged/gwb2ged.exe $(DISTRIB_DIR)/gw/gwb2ged$(EXT)
	cp $(BUILD_DISTRIB_DIR)cache_files/cache_files.exe $(DISTRIB_DIR)/gw/cache_files$(EXT)
	cp $(BUILD_DISTRIB_DIR)gwc/gwc.exe $(DISTRIB_DIR)/gw/gwc$(EXT)
	cp $(BUILD_DISTRIB_DIR)gwd/gwd.exe $(DISTRIB_DIR)/gw/gwd$(EXT)
	cp $(BUILD_DISTRIB_DIR)gwdiff/gwdiff.exe $(DISTRIB_DIR)/gw/gwdiff$(EXT)
	if test -f $(BUILD_DISTRIB_DIR)gwrepl/gwrepl.bc ; then cp $(BUILD_DISTRIB_DIR)gwrepl/gwrepl.bc $(DISTRIB_DIR)/gw/gwrepl$(EXT); fi
	cp $(BUILD_DISTRIB_DIR)gwu/gwu.exe $(DISTRIB_DIR)/gw/gwu$(EXT)
	cp $(BUILD_DISTRIB_DIR)setup/setup.exe $(DISTRIB_DIR)/gw/gwsetup$(EXT)
	cp $(BUILD_DISTRIB_DIR)update_nldb/update_nldb.exe $(DISTRIB_DIR)/gw/update_nldb$(EXT)
	@printf "\n\033[1;37m└ Copy templates in $(DISTRIB_DIR)/gw/\033[0m\n"
	cp -R hd/* $(DISTRIB_DIR)/gw/
	mkdir $(DISTRIB_DIR)/gw/setup
	cp bin/setup/intro.txt $(DISTRIB_DIR)/gw/setup/
	mkdir $(DISTRIB_DIR)/gw/setup/lang
	cp bin/setup/setup.gwf $(DISTRIB_DIR)/gw/setup/
	cp bin/setup/setup.css $(DISTRIB_DIR)/gw/setup/
	cp bin/setup/lang/*.htm $(DISTRIB_DIR)/gw/setup/lang/
	cp bin/setup/lang/lexicon.txt $(DISTRIB_DIR)/gw/setup/lang/
	cp bin/setup/lang/intro.txt $(DISTRIB_DIR)/gw/setup/lang/
	@printf "\n\033[1;37m└ Copy plugins in $(DISTRIB_DIR)/gw/plugins\033[0m\n"
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
	@printf "\033[1;37mBuild complete.\033[0m\n"
	@printf "You can launch Geneweb with “\033[1;37mcd $(DISTRIB_DIR)\033[0m” followed by “\033[1;37mgw/gwd$(EXT)\033[0m”.\n"
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
ci:
	@ocaml ./configure.ml && $(MAKE) -s clean build && GENEWEB_CI=on dune runtest
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
