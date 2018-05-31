ROOT := ..

INCDIRS := $(ROOT)/wserver $(ROOT)/dag2html
ALL_OBJS := ansel
ALL_EXE := gwc1 gwc2 mk_consang gwd gwu update_nldb
EVERYTHING_EXE := check_base
gwd_PREPEND_OBJS := gwlib
gwd_APPEND_OBJS := compilation
LIBS = unix
LINKFLAGS = -I $(CAMLP5D)

include $(ROOT)/tools/Makefile.common
-include $(ROOT)/src/Makefile.local

compilation.ml: always
	echo "Util.set_compilation_time \"$$(date "+%d %B %Y")\";" > $@
	echo "Util.set_commit \"$$(git show -s --pretty=format:%h)\";" >> $@
	echo "Util.set_commit_date \"$$(git show -s --pretty=format:%cd --date=format:'%d %B %Y')\";" >> $@

gwlib.ml:
	echo "value prefix =" > $@
	echo "  try Sys.getenv \"GWPREFIX\" with" >> $@
	echo "  [ Not_found -> \"$(PREFIX)\" ]" | sed -e 's|\\|/|g' >> $@
	echo ";" >> $@

api_link.cmx api_link.cmo: OCAMLFLAGS += -package yojson -package redis-sync

gwd.out gwd.opt: OCAMLFLAGS += -package piqirun.ext -package curl -package redis-sync -package yojson
gwd.out gwd.opt: LIBS = str gramlib #removes unix

i18n_check.out: i18n_check.cmo
	$(OCAMLC) i18n_check.cmo -o i18n_check.out

i18n: pr_transl.cmo always
	if test -f i18n; then mv i18n i18n.bak; fi
	export LC_ALL=C; \
	OBJS=`(grep -w -c "transl conf" *.ml; grep -w -c "transl_nth conf" *.ml) | grep -v :0 | sed s/:.*$$// | sort | uniq`; \
	( \
	  for i in $$OBJS; do \
	    echo $$i 1>&2; \
	    camlp5r pa_macro.cmo ./pa_lock.cmo ./pa_html.cmo ./pr_transl.cmo pa_extend.cmo $$i; \
	  done; \
	  cat ../hd/etc/*.txt ../hd/lang/start_utf8.txt ../hd/lang/advanced.txt | sed -e 's/%\[//g' -e 's/%]//g' -e 's/\]/\]\n/g' | grep "\[" | sed -e "s/^.*\[\*\?//" -e "s/::.*$$//" -e "s/].*$$//" \
	) | \
	sort | uniq | grep -v '^$$' > i18n

# Generation of *_piqi.ml files
.PHONY: piqi
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

clean:
	$(RM) gwlib.ml compilation.ml
