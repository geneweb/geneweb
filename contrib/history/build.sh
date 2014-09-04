#!/bin/bash

camlp5r ../../wserver/pa_macro5.cmo -DUNIX -o old_adef.ppi old_adef.mli
ocamlc.opt -warn-error A -g -I ../../wserver -I ../../dag2html -I ~/.opam/system/lib/camlp5/ -c -intf old_adef.ppi
/bin/rm -f old_adef.ppi
camlp5r ../../wserver/pa_macro5.cmo -DUNIX -o old_adef.ppo old_adef.ml
ocamlopt.opt -warn-error A -annot -g -I ../../wserver -I ../../dag2html -I ~/.opam/system/lib/camlp5/ -c -impl old_adef.ppo
/bin/rm -f old_adef.ppo
camlp5r ../../wserver/pa_macro5.cmo -DUNIX -o old_def.ppi old_def.mli
ocamlc.opt -warn-error A -g -I ../../wserver -I ../../dag2html -I ~/.opam/system/lib/camlp5/ -c -intf old_def.ppi
/bin/rm -f old_def.ppi

ocamlopt.opt -c -pp camlp5r -I ../../src is_gw_plus.ml
ocamlopt.opt unix.cmxa ../../src/secure.cmx ../../src/progrBar.cmx old_adef.cmx is_gw_plus.cmx -o is_gw_plus

ocamlopt.opt -c -pp camlp5r -I ../../src fix_hist.ml
ocamlopt.opt unix.cmxa ../../src/secure.cmx ../../src/progrBar.cmx old_adef.cmx fix_hist.cmx -o fix_hist
