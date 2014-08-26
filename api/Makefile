# $Id: Makefile,v 5.18 2007-09-12 09:58:44 ddr Exp $


CAMLP5F=-DUNIX
CAMLP5D=~/.opam/system/lib/camlp5/
MYSQLD=~/.opam/system/lib/mysql/
STRIP=strip
RM=/bin/rm -f

all:: opt

opt::
	cd service; $(MAKE) opt
	$(MAKE) api.opt
	$(RM) api
	cp gwd_api.opt gwd_api
	$(STRIP) gwd_api

clean::
	cd service; $(MAKE) clean
	$(RM) *.cm[oixa] *.cmxa *.pp[oi] *.o *.a *.obj *.lib *.lck *.bak *~ .#* *.annot .depend
	$(RM) gwd_api gwd_api.opt

api.opt:
	ocamlfind ocamlopt -annot -g -pp "camlp5r ../wserver/pa_macro5.cmo pa_extend.cmo ../src/pa_html.cmo ../src/pa_lock.cmo $(CAMLP5F)" -I ../src -I ./service -I ../wserver -c api_request.ml
	ocamlfind ocamlopt -annot -g -I $(CAMLP5D) -pp "camlp5r ../wserver/pa_macro5.cmo pa_extend.cmo ../src/pa_html.cmo ../src/pa_lock.cmo $(CAMLP5F)" -I ../src -I ../wserver -c api_server.ml
	ocamlfind ocamlopt -package piqi.lib -linkpkg unix.cmxa str.cmxa -I $(CAMLP5D) gramlib.cmxa -I ../src -I ./service -I ../wserver api_piqi.cmx api_piqi_ext.cmx api_app_piqi.cmx api_app_piqi_ext.cmx api_saisie_read_piqi.cmx api_saisie_read_piqi_ext.cmx api_saisie_write_piqi.cmx api_saisie_write_piqi_ext.cmx ../wserver/wserver.cmx ../wserver/wserverApi.cmx ../dag2html/dag2html.cmx ../src/argl.cmx ../src/lock.cmx ../src/adef.cmx ../src/iovalue.cmx ../src/buff.cmx ../src/name.cmx ../src/mutil.cmx ../src/secure.cmx ../src/btree.cmx ../src/futil.cmx ../src/dutil.cmx ../src/database.cmx ../src/db2.cmx ../src/db2disk.cmx ../src/diff.cmx ../src/gwdb.cmx ../src/progrBar.cmx ../src/consang.cmx ../src/checkItem.cmx ../src/check.cmx ../src/gutil.cmx ../src/pqueue.cmx ../src/num.cmx ../src/version.cmx ../src/translate.cmx ../src/gwlib.cmx ../src/notesLinks.cmx ../src/util.cmx ../src/calendar.cmx ../src/gwcomp.cmx ../src/templDate.cmx ../src/templ.cmx ../src/hutil.cmx ../src/date.cmx ../src/update.cmx ../src/wiki.cmx ../src/api_wiki.cmx ../src/history_diff.cmx ../src/history.cmx ../src/notes.cmx ../src/wiznotes.cmx ../src/srcfile.cmx ../src/dag.cmx ../src/perso.cmx ../src/updateInd.cmx ../src/updateIndOk.cmx ../src/updateFam.cmx ../src/updateFamOk.cmx ../src/place.cmx ../src/updateData.cmx ../src/relationLink.cmx ../src/relation.cmx ../src/doc.cmx ../src/forum.cmx ../src/image.cmx ../src/base64.cmx ../src/robot.cmx ../src/some.cmx metaphone.cmx api_def.cmx api_util.cmx api_search.cmx api_graph.cmx api_warnings.cmx api_saisie_read.cmx api.cmx api_update_util.cmx api_update_person.cmx api_update_family.cmx api_saisie_autocomplete.cmx api_saisie_write.cmx api_db.cmx api_request.cmx api_server.cmx -o gwd_api.opt
#	ocamlfind ocamlopt -package piqi.lib -linkpkg -I +camlp5 gramlib.cmxa -I ../src -I ./service -I ../wserver api_piqi.cmx api_piqi_ext.cmx api.cmx api_request.cmx api_server.cmx -o api.opt
	ocamlfind ocamlopt -package piqi.lib -linkpkg unix.cmxa str.cmxa -I $(CAMLP5D) gramlib.cmxa -I ../src -I ./service -I ../wserver api_piqi.cmx api_piqi_ext.cmx api_app_piqi.cmx api_app_piqi_ext.cmx api_saisie_read_piqi.cmx api_saisie_read_piqi_ext.cmx api_saisie_write_piqi.cmx api_saisie_write_piqi_ext.cmx ../wserver/wserver.cmx ../wserver/wserverApi.cmx ../dag2html/dag2html.cmx ../src/argl.cmx ../src/lock.cmx ../src/adef.cmx ../src/iovalue.cmx ../src/buff.cmx ../src/name.cmx ../src/mutil.cmx ../src/secure.cmx ../src/btree.cmx ../src/futil.cmx ../src/dutil.cmx ../src/database.cmx ../src/db2.cmx ../src/db2disk.cmx ../src/diff.cmx ../src/gwdb.cmx ../src/progrBar.cmx ../src/consang.cmx ../src/checkItem.cmx ../src/check.cmx ../src/gutil.cmx ../src/pqueue.cmx ../src/num.cmx ../src/version.cmx ../src/translate.cmx ../src/gwlib.cmx ../src/notesLinks.cmx ../src/util.cmx ../src/calendar.cmx ../src/gwcomp.cmx ../src/templDate.cmx ../src/templ.cmx ../src/hutil.cmx ../src/date.cmx ../src/update.cmx ../src/wiki.cmx ../src/api_wiki.cmx ../src/history_diff.cmx ../src/history.cmx ../src/notes.cmx ../src/wiznotes.cmx ../src/srcfile.cmx ../src/dag.cmx ../src/perso.cmx ../src/updateInd.cmx ../src/updateIndOk.cmx ../src/updateFam.cmx ../src/updateFamOk.cmx ../src/place.cmx ../src/updateData.cmx ../src/relationLink.cmx ../src/relation.cmx ../src/doc.cmx ../src/forum.cmx ../src/image.cmx ../src/base64.cmx ../src/robot.cmx ../src/some.cmx metaphone.cmx api_def.cmx api_util.cmx api_search.cmx api_graph.cmx api_warnings.cmx api_saisie_read.cmx api.cmx api_update_util.cmx api_update_person.cmx api_update_family.cmx api_saisie_autocomplete.cmx api_saisie_write.cmx api_db.cmx service/convert_dico_place.cmx -o service/convert_dico_place.opt

depend:
#	ocamlfind ocamldep -pp "camlp5r ../wserver/pa_macro5.cmo  ../src/pa_lock.cmo ../src/pa_html.cmo" -I ../src api_request.ml > .depend
	ocamlfind ocamldep -pp "camlp5r ../wserver/pa_macro5.cmo pa_extend.cmo ../src/def_syn.cmo ../src/pa_html.cmo ../src/pa_lock.cmo" -I ../src -I ../wserver *.mli *.ml > .depend

-include .depend
