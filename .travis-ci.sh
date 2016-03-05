set -uex

# Package dependencies
PKG_DEP=ocamlfind camlp5

# Install packages
opam install $PKG_DEP

# Build GeneWeb
./configure
make
make distrib
