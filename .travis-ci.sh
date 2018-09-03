set -uex
export OPAMYES=1
eval $(opam config env)
opam pin add geneweb . --no-action
opam install geneweb --deps-only
./configure && make install uninstall clean
./configure --api && make install uninstall clean
opam depext lablgtk && opam install lablgtk && make everything-exe
