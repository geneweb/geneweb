set -uex

# Install packages dependencies
export OPAMYES=1
opam install ocamlfind camlp5

# Build GeneWeb
./configure
make
make distrib
