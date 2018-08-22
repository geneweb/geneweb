set -uex
export OPAMYES=1
eval $(opam config env)
opam pin add geneweb . --no-action
opam install geneweb
