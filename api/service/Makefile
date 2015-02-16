# $Id: Makefile,v 0.1 2007-09-12 09:58:44 Exp $


CAMLP5F=-DUNIX
CAMLP5D=~/.opam/system/lib/camlp5/
MYSQLD=~/.opam/system/lib/mysql/
OCURL=~/.opam/system/lib/ocurl/
OCAMLOPT=ocamlfind ocamlopt -warn-error A -annot

STRIP=strip
RM=/bin/rm -f

all:: opt

out:: api.out

opt:: api.opt

clean::
	$(RM) *.cm[oixa] *.cmxa *.o *.a *.obj *.lib *.lck *.bak *~ .#* *.annot .depend

api.out:
	ocamlfind ocamlc -package piqi.lib -c api_piqi.ml
	ocamlfind ocamlc -package piqi.lib -c api_piqi_ext.ml
	ocamlfind ocamlc -package piqi.lib -c api_app_piqi.ml
	ocamlfind ocamlc -package piqi.lib -c api_app_piqi_ext.ml
	ocamlfind ocamlc -package piqi.lib -c api_saisie_read_piqi.ml
	ocamlfind ocamlc -package piqi.lib -c api_saisie_read_piqi_ext.ml
	ocamlfind ocamlc -package piqi.lib -c api_saisie_write_piqi.ml
	ocamlfind ocamlc -package piqi.lib -c api_saisie_write_piqi_ext.ml
	ocamlfind ocamlc -package piqi.lib -c -pp "camlp4o -I ./lib/ocaml/piqi pa_labelscope.cmo pa_openin.cmo" -I ../../src -I ../../wserver metaphone.ml
	ocamlfind ocamlc -package piqi.lib -c -pp "camlp4o -I ./lib/ocaml/piqi pa_labelscope.cmo pa_openin.cmo" -I ../../src -I ../../wserver api_def.ml
	ocamlfind ocamlc -package piqi.lib -c -pp "camlp4o -I ./lib/ocaml/piqi pa_labelscope.cmo pa_openin.cmo" -I ../../src -I ../../wserver api_util.ml
	ocamlfind ocamlc -package piqi.lib -c -pp "camlp4o -I ./lib/ocaml/piqi pa_labelscope.cmo pa_openin.cmo" -I ../../src -I ../../wserver api_search.ml
	ocamlfind ocamlc -package piqi.lib -c -pp "camlp4o -I ./lib/ocaml/piqi pa_labelscope.cmo pa_openin.cmo" -I ../../src -I ../../wserver api_graph.ml
	ocamlfind ocamlc -package piqi.lib -c -pp "camlp4o -I ./lib/ocaml/piqi pa_labelscope.cmo pa_openin.cmo" -I ../../src -I ../../wserver api_warnings.ml
	ocamlfind ocamlc -package piqi.lib -c -pp "camlp4o -I ./lib/ocaml/piqi pa_labelscope.cmo pa_openin.cmo" -I ../../src -I ../../wserver api.ml
	ocamlfind ocamlc -package piqi.lib -c -pp "camlp4o -I ./lib/ocaml/piqi pa_labelscope.cmo pa_openin.cmo" -I ../../src -I ../../wserver api_saisie_read.ml
	ocamlfind ocamlc -package piqi.lib -c -pp "camlp4o -I ./lib/ocaml/piqi pa_labelscope.cmo pa_openin.cmo" -I ../../src -I ../../wserver api_update_util.ml
	ocamlfind ocamlc -package piqi.lib -c -pp "camlp4o -I ./lib/ocaml/piqi pa_labelscope.cmo pa_openin.cmo" -I ../../src -I ../../wserver api_update_person.ml
	ocamlfind ocamlc -package piqi.lib -c -pp "camlp4o -I ./lib/ocaml/piqi pa_labelscope.cmo pa_openin.cmo" -I ../../src -I ../../wserver api_update_family.ml
	ocamlfind ocamlc -package piqi.lib -c -pp "camlp4o -I ./lib/ocaml/piqi pa_labelscope.cmo pa_openin.cmo" -I ../../src -I ../../wserver api_saisie_write.ml
	ocamlfind ocamlc -package piqi.lib -package mysql -linkpkg -noautolink -cclib '-Wl,-Bstatic -lmysql_stubs -lmysql -Wl,-Bdynamic -lz' -I $(MYSQLD) mysql.cmxa -annot -c -pp "camlp4o -I ./lib/ocaml/piqi pa_labelscope.cmo pa_openin.cmo" -I ../../src -I ../../wserver api_db.ml

api.opt:
	$(OCAMLOPT) -package piqi.lib -c api_piqi.ml
	$(OCAMLOPT) -package piqi.lib -c api_piqi_ext.ml
	$(OCAMLOPT) -package piqi.lib -c api_app_piqi.ml
	$(OCAMLOPT) -package piqi.lib -c api_app_piqi_ext.ml
	$(OCAMLOPT) -package piqi.lib -c api_saisie_read_piqi.ml
	$(OCAMLOPT) -package piqi.lib -c api_saisie_read_piqi_ext.ml
	$(OCAMLOPT) -package piqi.lib -c api_saisie_write_piqi.ml
	$(OCAMLOPT) -package piqi.lib -c api_saisie_write_piqi_ext.ml
	$(OCAMLOPT) -package piqi.lib -c -pp "camlp4o -I ./lib/ocaml/piqi pa_labelscope.cmo pa_openin.cmo" -I ../../src -I ../../wserver metaphone.ml
	$(OCAMLOPT) -package piqi.lib -c -pp "camlp4o -I ./lib/ocaml/piqi pa_labelscope.cmo pa_openin.cmo" -I ../../src -I ../../wserver api_def.ml
	$(OCAMLOPT) -package piqi.lib -c -pp "camlp4o -I ./lib/ocaml/piqi pa_labelscope.cmo pa_openin.cmo" -I ../../src -I ../../wserver api_util.ml
	$(OCAMLOPT) -package piqi.lib -c -pp "camlp4o -I ./lib/ocaml/piqi pa_labelscope.cmo pa_openin.cmo" -I ../../src -I ../../wserver api_search.ml
	$(OCAMLOPT) -package piqi.lib -c -pp "camlp4o -I ./lib/ocaml/piqi pa_labelscope.cmo pa_openin.cmo" -I ../../src -I ../../wserver api_graph.ml
	$(OCAMLOPT) -package piqi.lib -c -pp "camlp4o -I ./lib/ocaml/piqi pa_labelscope.cmo pa_openin.cmo" -I ../../src -I ../../wserver api_warnings.ml
	$(OCAMLOPT) -package piqi.lib -c -pp "camlp4o -I ./lib/ocaml/piqi pa_labelscope.cmo pa_openin.cmo" -I ../../src -I ../../wserver api.ml
	$(OCAMLOPT) -package piqi.lib -c -pp "camlp4o -I ./lib/ocaml/piqi pa_labelscope.cmo pa_openin.cmo" -I ../../src -I ../../wserver api_saisie_read.ml
	$(OCAMLOPT) -package piqi.lib -c -pp "camlp4o -I ./lib/ocaml/piqi pa_labelscope.cmo pa_openin.cmo" -I ../../src -I ../../wserver api_update_util.ml
	$(OCAMLOPT) -package piqi.lib -c -pp "camlp4o -I ./lib/ocaml/piqi pa_labelscope.cmo pa_openin.cmo" -I ../../src -I ../../wserver api_update_person.ml
	$(OCAMLOPT) -package piqi.lib -c -pp "camlp4o -I ./lib/ocaml/piqi pa_labelscope.cmo pa_openin.cmo" -I ../../src -I ../../wserver api_update_family.ml
	$(OCAMLOPT) -c -pp "camlp5r ../../wserver/pa_macro5.cmo pa_extend.cmo ../../src/pa_lock.cmo $(CAMLP5F)" -I ../../src api_saisie_autocomplete.ml
	$(OCAMLOPT) -package piqi.lib -c -pp "camlp4o -I ./lib/ocaml/piqi pa_labelscope.cmo pa_openin.cmo" -I ../../src -I ../../wserver api_saisie_write.ml
	$(OCAMLOPT) -package piqi.lib -package mysql -linkpkg -noautolink -cclib '-Wl,-Bstatic -lmysql_stubs -lmysql -Wl,-Bdynamic -lz' -I $(MYSQLD) mysql.cmxa -c -pp "camlp4o -I ./lib/ocaml/piqi pa_labelscope.cmo pa_openin.cmo" -I ../../src -I ../../wserver api_db.ml
	$(OCAMLOPT) -package piqi.lib -c -pp "camlp4o -I ./lib/ocaml/piqi pa_labelscope.cmo pa_openin.cmo" -I ../../src -I ../../wserver convert_dico_place.ml


piqi:
	piqi of-proto --normalize api.proto
	piqic ocaml-ext --pp api.proto.piqi
	piqi of-proto --normalize api_app.proto
	piqic ocaml-ext --pp api_app.proto.piqi
	piqi of-proto --normalize api_saisie_read.proto
	piqic ocaml-ext --pp api_saisie_read.proto.piqi
	piqi of-proto --normalize api_saisie_write.proto
	piqic ocaml-ext --pp api_saisie_write.proto.piqi
	$(RM) *.piqi

depend:
#	ocamlfind ocamldep api_piqi.ml > .depend
#	ocamlfind ocamldep api_piqi_ext.ml >> .depend
	ocamlfind ocamldep -pp "camlp4o -I ./lib/ocaml/piqi pa_labelscope.cmo pa_openin.cmo" -I ../../src -I ../../wserver *.mli *.ml > .depend

-include .depend
