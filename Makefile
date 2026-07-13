DUNE?=dune
DOCKER?=docker
ODOC_DIR?=_build/default/_doc/_html
OPAM?=opam
TAG?=dev

fmt:
	$(DUNE) fmt

build:
	$(DUNE) build @install

build-geneweb:
	$(DUNE) build @bin/all @lib/all

build-geneweb-rpc:
	$(DUNE) build @rpc/all

build-gwd:
	$(DUNE) build @bin/gwd/all @bin/gwc/all

bundle:
	TAG="$(TAG)" ./scripts/generate_bundle.sh

bundle-static:
	TAG="$(TAG)" LINKING_MODE=static ./scripts/generate_bundle.sh

deps:
	$(OPAM) pin oui \
		'https://github.com/Halbaroth/ocaml-universal-installer.git#fix-win-ldd'
	$(OPAM) pin ancient \
		'https://github.com/Halbaroth/ocaml-ancient.git#windows-support-2'
	$(OPAM) install . --deps-only --with-test

docker:
	$(DOCKER) build -t gwd --target gwd .
	$(DOCKER) build -t gwsetup --target gwsetup .

static:
	$(DOCKER) build \
		--build-arg TAG="$(TAG)" \
		-t geneweb-static -f Dockerfile.static . -o .

doc:
	$(DUNE) build @doc

opendoc: doc
	xdg-open $(ODOC_DIR)/index.html

test: build
	$(DUNE) runtest

ci: build
	GENEWEB_CI=on $(DUNE) build @runtest

bench:
	$(DUNE) build @runbench

clean:
	$(DUNE) clean

.PHONY: all fmt build build-geneweb build-geneweb-rpc bundle bundle-static \
	doc opendoc test ci bench clean
