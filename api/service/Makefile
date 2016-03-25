# $Id: Makefile,v 0.1 2007-09-12 09:58:44 Exp $

#include ../../tools/Makefile.ocaml

CAMLP5F=-DUNIX
OCAMLOPT=ocamlfind ocamlopt -warn-error A -safe-string -annot -g
STRIP=strip
RM=/bin/rm -f

all:: opt

out:: api.out

opt:: api.opt

clean::
	$(RM) *.cm[oixa] *.cmxa *.o *.a *.obj *.lib *.lck *.bak *~ .#* *.annot .depend

api.out:
	ocamlfind ocamlc -package piqirun.ext -c api_piqi.ml
	ocamlfind ocamlc -package piqirun.ext -c api_piqi_ext.ml
	ocamlfind ocamlc -package piqirun.ext -c api_app_piqi.ml
	ocamlfind ocamlc -package piqirun.ext -c api_app_piqi_ext.ml
	ocamlfind ocamlc -package piqirun.ext -c api_saisie_read_piqi.ml
	ocamlfind ocamlc -package piqirun.ext -c api_saisie_read_piqi_ext.ml
	ocamlfind ocamlc -package piqirun.ext -c api_saisie_write_piqi.ml
	ocamlfind ocamlc -package piqirun.ext -c api_saisie_write_piqi_ext.ml
	ocamlfind ocamlc -package piqirun.ext -c -I ../../src -I ../../wserver metaphone.ml
	ocamlfind ocamlc -package piqirun.ext -c -I ../../src -I ../../wserver api_def.ml
	ocamlfind ocamlc -package piqirun.ext -c -I ../../src -I ../../wserver api_util.ml
	ocamlfind ocamlc -package piqirun.ext -c -I ../../src -I ../../wserver api_search.ml
	ocamlfind ocamlc -package piqirun.ext -c -I ../../src -I ../../wserver api_graph.ml
	ocamlfind ocamlc -package piqirun.ext -c -I ../../src -I ../../wserver api_warnings.ml
	ocamlfind ocamlc -package piqirun.ext -c -I ../../src -I ../../wserver api_stats.ml
	ocamlfind ocamlc -package piqirun.ext -c -I ../../src -I ../../wserver api.ml
	ocamlfind ocamlc -package piqirun.ext -c -I ../../src -I ../../wserver api_saisie_read.ml
	ocamlfind ocamlc -package piqirun.ext -c -I ../../src -I ../../wserver api_update_util.ml
	ocamlfind ocamlc -package piqirun.ext -c -I ../../src -I ../../wserver api_update_person.ml
	ocamlfind ocamlc -package piqirun.ext -c -I ../../src -I ../../wserver api_update_family.ml
	ocamlfind ocamlc -package piqirun.ext -c -I ../../src -I ../../wserver api_saisie_write.ml

api.opt:
	$(OCAMLOPT) -package piqirun.ext -c api_piqi.ml
	$(OCAMLOPT) -package piqirun.ext -c api_piqi_ext.ml
	$(OCAMLOPT) -package piqirun.ext -c api_stats_piqi.ml
	$(OCAMLOPT) -package piqirun.ext -c api_stats_piqi_ext.ml
	$(OCAMLOPT) -package piqirun.ext -c api_app_piqi.ml
	$(OCAMLOPT) -package piqirun.ext -c api_app_piqi_ext.ml
	$(OCAMLOPT) -package piqirun.ext -c api_saisie_read_piqi.ml
	$(OCAMLOPT) -package piqirun.ext -c api_saisie_read_piqi_ext.ml
	$(OCAMLOPT) -package piqirun.ext -c api_saisie_write_piqi.ml
	$(OCAMLOPT) -package piqirun.ext -c api_saisie_write_piqi_ext.ml
# Compilé dans le Makefile de src
#	$(OCAMLOPT) -package piqirun.ext -c api_link_tree_piqi.ml
#	$(OCAMLOPT) -package piqirun.ext -c api_link_tree_piqi_ext.ml
	$(OCAMLOPT) -package piqirun.ext -c -I ../../src -I ../../wserver metaphone.ml
	$(OCAMLOPT) -package piqirun.ext -c -I ../../src -I ../../wserver api_def.ml
	$(OCAMLOPT) -package piqirun.ext -c -I ../../src -I ../../wserver api_util.ml
	$(OCAMLOPT) -package piqirun.ext -c -I ../../src -I ../../wserver api_search.ml
	$(OCAMLOPT) -package piqirun.ext -c -I ../../src -I ../../wserver api_graph.ml
	$(OCAMLOPT) -package piqirun.ext -c -I ../../src -I ../../wserver api_warnings.ml
	$(OCAMLOPT) -package piqirun.ext -c -I ../../src -I ../../wserver api_stats.ml
	$(OCAMLOPT) -package piqirun.ext -c -I ../../src -I ../../wserver api.ml
	$(OCAMLOPT) -package piqirun.ext -c -I ../../src -I ../../wserver api_saisie_read.ml
	$(OCAMLOPT) -package piqirun.ext -c -I ../../src -I ../../wserver api_update_util.ml
	$(OCAMLOPT) -package piqirun.ext -c -I ../../src -I ../../wserver api_update_person.ml
	$(OCAMLOPT) -package piqirun.ext -c -I ../../src -I ../../wserver api_update_family.ml
	$(OCAMLOPT) -c -pp "camlp5r pa_macro.cmo pa_extend.cmo ../../src/pa_lock.cmo $(CAMLP5F)" -I ../../src api_saisie_autocomplete.ml
	$(OCAMLOPT) -package piqirun.ext -c -I ../../src -I ../../wserver api_saisie_write.ml
	$(OCAMLOPT) -package piqirun.ext -package curl -I $(OCURL) curl.cmxa -I $(LREDIS) -package yojson -I ../../src -I ../../wserver -c api_link.ml
	$(OCAMLOPT) -package piqirun.ext -c -I ../../src -I ../../wserver convert_dico_place.ml


piqi:
	piqi of-proto --normalize api.proto
	piqic-ocaml --ext api.proto.piqi
	@echo -e "(* nocamlp5 *)\n" | cat - api_piqi.ml > tmp.ml && mv tmp api_piqi.ml
	@echo -e "(* nocamlp5 *)\n" | cat - api_piqi_ext.ml > tmp.ml && mv tmp app_piqi_ext.ml
	piqi of-proto --normalize api_app.proto
	piqic-ocaml --ext api_app.proto.piqi
	@echo -e "(* nocamlp5 *)\n" | cat - api_app_piqi.ml > tmp.ml && mv tmp api_app_piqi.ml
	@echo -e "(* nocamlp5 *)\n" | cat - api_app_piqi_ext.ml > tmp.ml && mv tmp api_app_piqi_ext.ml
	piqi of-proto --normalize api_saisie_read.proto
	piqic-ocaml --ext api_saisie_read.proto.piqi
	@echo -e "(* nocamlp5 *)\n" | cat - api_saisie_read_piqi.ml > tmp.ml && mv tmp api_saisie_read_piqi.ml
	@echo -e "(* nocamlp5 *)\n" | cat - api_saisie_read_piqi_ext.ml > tmp.ml && mv tmp api__saisie_read_piqi_ext.ml
	piqi of-proto --normalize api_saisie_write.proto
	piqic-ocaml --ext api_saisie_write.proto.piqi
	@echo -e "(* nocamlp5 *)\n" | cat - api_saise_write_piqi.ml > tmp.ml && mv tmp api_saisie_write_piqi.ml
	@echo -e "(* nocamlp5 *)\n" | cat - api_saise_write_piqi_ext.ml > tmp.ml && mv tmp api_saisie_write_piqi_ext.ml
	piqi of-proto --normalize api_link_tree.proto
	piqic-ocaml --ext api_link_tree.proto.piqi
	@echo -e "(* nocamlp5 *)\n" | cat - api_link_tree_piqi.ml > tmp.ml && mv tmp api_link_tree_piqi.ml
	@echo -e "(* nocamlp5 *)\n" | cat - api_link_tree_piqi_ext.ml > tmp.ml && mv tmp api_link_tree_piqi_ext.ml
	piqi of-proto --normalize api_stats.proto
	piqic-ocaml --ext api_stats.proto.piqi
	@echo -e "(* nocamlp5 *)\n" | cat - api_stats_piqi.ml > tmp.ml && mv tmp api_stats_piqi.ml
	@echo -e "(* nocamlp5 *)\n" | cat - api_stats_piqi_ext.ml > tmp.ml && mv tmp api_stats_piqi_ext.ml
	$(RM) *.piqi

depend:
#	ocamlfind ocamldep api_piqi.ml > .depend
#	ocamlfind ocamldep api_piqi_ext.ml >> .depend
	ocamlfind ocamldep -I ../../src -I ../../wserver *.mli *.ml > .depend

-include .depend
