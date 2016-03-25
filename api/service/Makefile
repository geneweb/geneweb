ROOT=../..
include $(ROOT)/tools/Makefile.config

OCAMLI=-I $(ROOT)/src -I $(ROOT)/wserver

.PHONY: out opt clean

out: api.out

opt: api.opt

clean:
	$(RM) *.cm[oixa] *.cmxa *.o *.a *.obj *.lib *.lck *.bak *~ .#* *.annot .depend

api.out:
	$(OCAMLC) -c api_piqi.ml
	$(OCAMLC) -c api_piqi_ext.ml
	$(OCAMLC) -c api_app_piqi.ml
	$(OCAMLC) -c api_app_piqi_ext.ml
	$(OCAMLC) -c api_saisie_read_piqi.ml
	$(OCAMLC) -c api_saisie_read_piqi_ext.ml
	$(OCAMLC) -c api_saisie_write_piqi.ml
	$(OCAMLC) -c api_saisie_write_piqi_ext.ml
	$(OCAMLC) -c $(OCAMLI) metaphone.ml
	$(OCAMLC) -c $(OCAMLI) api_def.ml
	$(OCAMLC) -c $(OCAMLI) api_util.ml
	$(OCAMLC) -c $(OCAMLI) api_search.ml
	$(OCAMLC) -c $(OCAMLI) api_graph.ml
	$(OCAMLC) -c $(OCAMLI) api_warnings.ml
	$(OCAMLC) -c $(OCAMLI) api_stats.ml
	$(OCAMLC) -c $(OCAMLI) api.ml
	$(OCAMLC) -c $(OCAMLI) api_saisie_read.ml
	$(OCAMLC) -c $(OCAMLI) api_update_util.ml
	$(OCAMLC) -c $(OCAMLI) api_update_person.ml
	$(OCAMLC) -c $(OCAMLI) api_update_family.ml
	$(OCAMLC) -c $(OCAMLI) api_saisie_write.ml

api.opt:
	$(OCAMLOPT) -c api_piqi.ml
	$(OCAMLOPT) -c api_piqi_ext.ml
	$(OCAMLOPT) -c api_stats_piqi.ml
	$(OCAMLOPT) -c api_stats_piqi_ext.ml
	$(OCAMLOPT) -c api_app_piqi.ml
	$(OCAMLOPT) -c api_app_piqi_ext.ml
	$(OCAMLOPT) -c api_saisie_read_piqi.ml
	$(OCAMLOPT) -c api_saisie_read_piqi_ext.ml
	$(OCAMLOPT) -c api_saisie_write_piqi.ml
	$(OCAMLOPT) -c api_saisie_write_piqi_ext.ml
# Compilé dans le Makefile de src
	$(OCAMLOPT) -c api_link_tree_piqi.ml
	$(OCAMLOPT) -c api_link_tree_piqi_ext.ml
	$(OCAMLOPT) -c $(OCAMLI) metaphone.ml
	$(OCAMLOPT) -c $(OCAMLI) api_def.ml
	$(OCAMLOPT) -c $(OCAMLI) api_util.ml
	$(OCAMLOPT) -c $(OCAMLI) api_search.ml
	$(OCAMLOPT) -c $(OCAMLI) api_graph.ml
	$(OCAMLOPT) -c $(OCAMLI) api_warnings.ml
	$(OCAMLOPT) -c $(OCAMLI) api_stats.ml
	$(OCAMLOPT) -c $(OCAMLI) api.ml
	$(OCAMLOPT) -c $(OCAMLI) api_saisie_read.ml
	$(OCAMLOPT) -c $(OCAMLI) api_update_util.ml
	$(OCAMLOPT) -c $(OCAMLI) api_update_person.ml
	$(OCAMLOPT) -c $(OCAMLI) api_update_family.ml
	$(OCAMLOPT) -c -pp "camlp5r pa_macro.cmo pa_extend.cmo ../../src/pa_lock.cmo $(CAMLP5F)" $(OCAMLI) api_saisie_autocomplete.ml
	$(OCAMLOPT) -c $(OCAMLI) api_saisie_write.ml
	$(OCAMLOPT) -package curl -I $(OCURL) curl.cmxa -I $(LREDIS) -package yojson $(OCAMLI) -c api_link.ml
	$(OCAMLOPT) -c $(OCAMLI) convert_dico_place.ml


piqi:
	piqi of-proto --normalize api.proto
	piqic-ocaml --ext api.proto.piqi
	@echo -e "(* nocamlp5 *)\n" | cat - api_piqi.ml > tmp && mv tmp api_piqi.ml
	@echo -e "(* nocamlp5 *)\n" | cat - api_piqi_ext.ml > tmp && mv tmp api_piqi_ext.ml
	piqi of-proto --normalize api_app.proto
	piqic-ocaml --ext api_app.proto.piqi
	@echo -e "(* nocamlp5 *)\n" | cat - api_app_piqi.ml > tmp && mv tmp api_app_piqi.ml
	@echo -e "(* nocamlp5 *)\n" | cat - api_app_piqi_ext.ml > tmp && mv tmp api_app_piqi_ext.ml
	piqi of-proto --normalize api_saisie_read.proto
	piqic-ocaml --ext api_saisie_read.proto.piqi
	@echo -e "(* nocamlp5 *)\n" | cat - api_saisie_read_piqi.ml > tmp && mv tmp api_saisie_read_piqi.ml
	@echo -e "(* nocamlp5 *)\n" | cat - api_saisie_read_piqi_ext.ml > tmp && mv tmp api_saisie_read_piqi_ext.ml
	piqi of-proto --normalize api_saisie_write.proto
	piqic-ocaml --ext api_saisie_write.proto.piqi
	@echo -e "(* nocamlp5 *)\n" | cat - api_saisie_write_piqi.ml > tmp && mv tmp api_saisie_write_piqi.ml
	@echo -e "(* nocamlp5 *)\n" | cat - api_saisie_write_piqi_ext.ml > tmp && mv tmp api_saisie_write_piqi_ext.ml
	piqi of-proto --normalize api_link_tree.proto
	piqic-ocaml --ext api_link_tree.proto.piqi
	@echo -e "(* nocamlp5 *)\n" | cat - api_link_tree_piqi.ml > tmp && mv tmp api_link_tree_piqi.ml
	@echo -e "(* nocamlp5 *)\n" | cat - api_link_tree_piqi_ext.ml > tmp && mv tmp api_link_tree_piqi_ext.ml
	piqi of-proto --normalize api_stats.proto
	piqic-ocaml --ext api_stats.proto.piqi
	@echo -e "(* nocamlp5 *)\n" | cat - api_stats_piqi.ml > tmp && mv tmp api_stats_piqi.ml
	@echo -e "(* nocamlp5 *)\n" | cat - api_stats_piqi_ext.ml > tmp && mv tmp api_stats_piqi_ext.ml
	$(RM) *.piqi

.PHONY: depend
depend:
#	ocamlfind ocamldep api_piqi.ml > .depend
#	ocamlfind ocamldep api_piqi_ext.ml >> .depend
#	ocamlfind ocamldep $(OCAMLI) *.mli *.ml > .depend
	$(ROOT)/tools/camlp5_depend.sh $(CAMLP5F) $(OCAMLI) -- *.ml* > .depend

.depend:
	@if [ ! -e $@ ]; then $(MAKE) -o $@ depend; fi

ifeq ($(filter $(MAKECMDGOALS),clean depend),)
  include .depend
endif
