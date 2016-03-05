set -uex

# Install packages dependencies
export OPAMYES=1
eval $(opam config env)
opam install ocamlfind camlp5

# Build GeneWeb
./configure
make
make distrib
