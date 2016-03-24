ROOT=..
include $(ROOT)/tools/Makefile.config

DEPEND_REDIS=utils.cmx mutex.cmx client.cmx cache.cmx redis_sync.cmx
OCAMLI=-I ../src -I ./service -I ../wserver

OBJS_API_GWD=api_stats_piqi.cmx api_stats_piqi_ext.cmx \
api_link_tree_piqi.cmx api_link_tree_piqi_ext.cmx api_piqi.cmx \
api_piqi_ext.cmx api_app_piqi.cmx api_app_piqi_ext.cmx \
api_saisie_read_piqi.cmx api_saisie_read_piqi_ext.cmx \
api_saisie_write_piqi.cmx api_saisie_write_piqi_ext.cmx \
../wserver/httpStatus.cmx ../wserver/wserver.cmx \
../dag2html/dag2html.cmx ../src/api_conf.cmx ../src/argl.cmx \
../src/lock.cmx ../src/adef.cmx ../src/iovalue.cmx ../src/buff.cmx \
../src/name.cmx ../src/mutil.cmx ../src/secure.cmx ../src/btree.cmx \
../src/futil.cmx ../src/dutil.cmx ../src/database.cmx ../src/db2.cmx \
../src/db2disk.cmx ../src/diff.cmx ../src/gwdb.cmx ../src/progrBar.cmx \
../src/consang.cmx ../src/checkItem.cmx ../src/check.cmx \
../src/gutil.cmx ../src/pqueue.cmx ../src/sosa.cmx ../src/version.cmx \
../src/translate.cmx ../src/gwlib.cmx ../src/notesLinks.cmx \
../src/util.cmx ../src/calendar.cmx ../src/gwcomp.cmx \
../src/templDate.cmx ../src/templ.cmx ../src/hutil.cmx ../src/date.cmx \
../src/update.cmx ../src/wiki.cmx ../src/api_wiki.cmx \
../src/history_diff.cmx ../src/history.cmx ../src/notes.cmx \
../src/wiznotes.cmx ../src/srcfile.cmx ../src/dag.cmx ../src/link.cmx \
../src/perso_link.cmx ../src/perso.cmx ../src/updateInd.cmx \
../src/updateIndOk.cmx ../src/updateFam.cmx ../src/updateFamOk.cmx \
../src/place.cmx ../src/updateData.cmx ../src/relationLink.cmx \
../src/relation.cmx ../src/forum.cmx ../src/image.cmx \
../src/base64.cmx ../src/robot.cmx ../src/alln.cmx ../src/some.cmx \
metaphone.cmx api_def.cmx api_util.cmx api_search.cmx api_graph.cmx \
api_warnings.cmx api_saisie_read.cmx api_stats.cmx api.cmx \
api_update_util.cmx api_update_person.cmx api_update_family.cmx \
api_saisie_autocomplete.cmx api_saisie_write.cmx api_link.cmx \
api_request.cmx api_server.cmx

OBJS_DICO_PLACE=api_stats_piqi.cmx api_stats_piqi_ext.cmx \
api_link_tree_piqi.cmx api_link_tree_piqi_ext.cmx api_piqi.cmx \
api_piqi_ext.cmx api_app_piqi.cmx api_app_piqi_ext.cmx \
api_saisie_read_piqi.cmx api_saisie_read_piqi_ext.cmx \
api_saisie_write_piqi.cmx api_saisie_write_piqi_ext.cmx \
../wserver/httpStatus.cmx ../wserver/wserver.cmx \
../dag2html/dag2html.cmx ../src/api_conf.cmx ../src/argl.cmx \
../src/lock.cmx ../src/adef.cmx ../src/iovalue.cmx ../src/buff.cmx \
../src/name.cmx ../src/mutil.cmx ../src/secure.cmx ../src/btree.cmx \
../src/futil.cmx ../src/dutil.cmx ../src/database.cmx ../src/db2.cmx \
../src/db2disk.cmx ../src/diff.cmx ../src/gwdb.cmx ../src/progrBar.cmx \
../src/consang.cmx ../src/checkItem.cmx ../src/check.cmx \
../src/gutil.cmx ../src/pqueue.cmx ../src/sosa.cmx ../src/version.cmx \
../src/translate.cmx ../src/gwlib.cmx ../src/notesLinks.cmx \
../src/util.cmx ../src/calendar.cmx ../src/gwcomp.cmx \
../src/templDate.cmx ../src/templ.cmx ../src/hutil.cmx ../src/date.cmx \
../src/update.cmx ../src/wiki.cmx ../src/api_wiki.cmx \
../src/history_diff.cmx ../src/history.cmx ../src/notes.cmx \
../src/wiznotes.cmx ../src/srcfile.cmx ../src/dag.cmx ../src/link.cmx \
../src/perso_link.cmx ../src/perso.cmx ../src/updateInd.cmx \
../src/updateIndOk.cmx ../src/updateFam.cmx ../src/updateFamOk.cmx \
../src/place.cmx ../src/updateData.cmx ../src/relationLink.cmx \
../src/relation.cmx ../src/forum.cmx ../src/image.cmx \
../src/base64.cmx ../src/robot.cmx ../src/alln.cmx ../src/some.cmx \
metaphone.cmx api_def.cmx api_util.cmx api_search.cmx api_graph.cmx \
api_warnings.cmx api_saisie_read.cmx api_stats.cmx api.cmx \
api_update_util.cmx api_update_person.cmx api_update_family.cmx \
api_saisie_autocomplete.cmx api_saisie_write.cmx api_link.cmx \
service/convert_dico_place.cmx

.PHONY: opt clean

opt:
	cd service; $(MAKE) opt
	$(MAKE) api.opt
	$(RM) api
	cp gwd_api.opt gwd_api
	$(STRIP) gwd_api

clean:
	cd service; $(MAKE) clean
	$(RM) *.cm[oixa] *.cmxa *.pp[oi] *.o *.a *.obj *.lib *.lck *.bak *~ .#* *.annot .depend
	$(RM) gwd_api gwd_api.opt

api.opt:
	$(OCAMLOPT) -pp "camlp5r pa_macro.cmo pa_extend.cmo ../src/pa_html.cmo ../src/pa_lock.cmo $(CAMLP5F)" $(OCAMLI) -c api_request.ml
	$(OCAMLOPT) -I $(CAMLP5D) -pp "camlp5r pa_extend.cmo ../src/pa_html.cmo ../src/pa_lock.cmo pa_macro.cmo $(CAMLP5F)" $(OCAMLI) api_link.cmx -c api_server.ml
	$(OCAMLOPT) -linkpkg str.cmxa -I $(CAMLP5D) gramlib.cmxa $(OCAMLI) -package curl -I $(OCURL) curl.cmxa -package re -I $(RE) re_emacs.cmxa re_str.cmxa -package uuidm -I $(LREDIS) $(DEPEND_REDIS) -package yojson -linkpkg $(OBJS_API_GWD) -o gwd_api.opt
#	ocamlfind ocamlopt -package piqi.lib -linkpkg -I +camlp5 gramlib.cmxa -I ../src -I ./service -I ../wserver api_piqi.cmx api_piqi_ext.cmx api.cmx api_request.cmx api_server.cmx -o api.opt
	$(OCAMLOPT) -linkpkg str.cmxa -I $(CAMLP5D) gramlib.cmxa $(OCAMLI) -package curl -I $(OCURL) curl.cmxa -package re -I $(RE) re_emacs.cmxa re_str.cmxa -package uuidm -I $(LREDIS) $(DEPEND_REDIS) -package yojson -linkpkg $(OBJS_DICO_PLACE) -o service/convert_dico_place.opt

depend:
# Nothing here
