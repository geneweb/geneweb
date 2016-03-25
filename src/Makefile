ROOT=..
include $(ROOT)/tools/Makefile.config

# First rule 'all' is defined in Makefile.config.

# Overwrite variables.
OCAMLI=-I ../wserver -I ../dag2html -I ../api/service

GWC_OBJS=argl.cmo lock.cmo adef.cmo iovalue.cmo buff.cmo name.cmo mutil.cmo secure.cmo btree.cmo futil.cmo dutil.cmo database.cmo db2.cmo db2disk.cmo diff.cmo outbase.cmo gwdb.cmo pqueue.cmo progrBar.cmo consang.cmo consangAll.cmo checkItem.cmo check.cmo calendar.cmo notesLinks.cmo gwcomp.cmo db1link.cmo gwc.cmo
GWC2_OBJS=calendar.cmo adef.cmo iovalue.cmo buff.cmo name.cmo mutil.cmo gwcomp.cmo argl.cmo lock.cmo progrBar.cmo secure.cmo iochan.cmo futil.cmo db2.cmo db2out.cmo notesLinks.cmo db2disk.cmo dutil.cmo btree.cmo database.cmo gwdb.cmo consang.cmo consangAll.cmo checkItem.cmo check.cmo db2link.cmo gwc2.cmo
CONSANG_OBJS=argl.cmo lock.cmo adef.cmo iovalue.cmo buff.cmo name.cmo mutil.cmo secure.cmo btree.cmo futil.cmo dutil.cmo progrBar.cmo database.cmo db2.cmo db2disk.cmo db2out.cmo diff.cmo outbase.cmo gwdb.cmo pqueue.cmo consang.cmo consangAll.cmo mk_consang.cmo
GWD_OBJS_API1=api_conf.cmo
GWD_OBJS_API2=../api/service/api_link_tree_piqi.cmo ../api/service/api_link_tree_piqi_ext.cmo link.cmo api_wiki.cmo perso_link.cmo
GWD_OBJS1=../wserver/httpStatus.cmo ../wserver/wserver.cmo ../dag2html/dag2html.cmo argl.cmo lock.cmo adef.cmo iovalue.cmo buff.cmo name.cmo mutil.cmo secure.cmo btree.cmo futil.cmo dutil.cmo database.cmo db2.cmo db2disk.cmo diff.cmo gwdb.cmo checkItem.cmo gutil.cmo pqueue.cmo consang.cmo num.cmo version.cmo translate.cmo gwlib.cmo notesLinks.cmo util.cmo calendar.cmo templDate.cmo templ.cmo hutil.cmo date.cmo update.cmo wiki.cmo history_diff.cmo history.cmo notes.cmo wiznotes.cmo srcfile.cmo dag.cmo
GWD_OBJS2=perso.cmo updateInd.cmo updateIndOk.cmo updateFam.cmo updateFamOk.cmo place.cmo updateData.cmo merge.cmo mergeFamOk.cmo mergeFam.cmo mergeInd.cmo mergeIndOk.cmo mergeDup.cmo changeChildren.cmo sendImage.cmo relationLink.cmo relation.cmo descend.cmo birthday.cmo cousins.cmo alln.cmo some.cmo searchName.cmo advSearchOk.cmo birthDeath.cmo title.cmo forum.cmo image.cmo request.cmo base64.cmo robot.cmo compilation.cmo gwd.cmo
ifeq ($(WITH_API),y)
  GWD_OBJS=$(GWD_OBJS_API1) $(GWD_OBJS1) $(GWD_OBJS_API2) $(GWD_OBJS2)
else
  GWD_OBJS=$(GWD_OBJS1) $(GWD_OBJS2)
endif
GWU_OBJS=argl.cmo adef.cmo iovalue.cmo buff.cmo name.cmo mutil.cmo secure.cmo btree.cmo futil.cmo dutil.cmo database.cmo db2.cmo db2disk.cmo diff.cmo gwdb.cmo checkItem.cmo gutil.cmo select.cmo calendar.cmo notesLinks.cmo progrBar.cmo gwu.cmo
PHONYGWD_OBJS=../wserver/wserver.cmo argl.cmo phonygwd.cmo
PROBOT_OBJS=probot.cmo
MOSTDESC_OBJS=argl.cmo secure.cmo buff.cmo name.cmo iovalue.cmo mutil.cmo adef.cmo futil.cmo diff.cmo dutil.cmo btree.cmo database.cmo db2.cmo db2disk.cmo gwdb.cmo consang.cmo pqueue.cmo num.cmo mostdesc.cmo
FPLA_OBJS=argl.cmo secure.cmo buff.cmo name.cmo iovalue.cmo mutil.cmo adef.cmo futil.cmo diff.cmo dutil.cmo btree.cmo database.cmo db2.cmo db2disk.cmo gwdb.cmo fpla.cmo
UPDATE_NLDB_OBJS=secure.cmo buff.cmo name.cmo adef.cmo iovalue.cmo mutil.cmo btree.cmo futil.cmo dutil.cmo database.cmo db2.cmo db2disk.cmo diff.cmo gwdb.cmo checkItem.cmo gutil.cmo argl.cmo notesLinks.cmo progrBar.cmo update_nldb.cmo
CHECK_BASE_OBJS=buff.cmo name.cmo iovalue.cmo mutil.cmo adef.cmo futil.cmo diff.cmo secure.cmo dutil.cmo btree.cmo database.cmo db2.cmo db2disk.cmo gwdb.cmo progrBar.cmo checkItem.cmo consang.cmo check.cmo check_base.cmo


.PHONY: out
out: ansel.cmo gwc1.out gwc2.out consang.out gwd.out gwu.out update_nldb.out check_base.out
	cp gwc1.out gwc
	cp gwc1.out gwc1
	cp gwc2.out gwc2
	cp consang.out consang
	cp gwd.out gwd
	cp gwu.out gwu
	cp update_nldb.out update_nldb
	cp check_base.out check_base

.PHONY: opt
ifeq ($(WITH_API),y)
opt: link.opt
endif
opt: ansel.cmx gwc1.opt gwc2.opt consang.opt gwd.opt gwu.opt update_nldb.opt check_base.opt
	cp gwc1.opt gwc
	cp gwc1.opt gwc1
	cp gwc2.opt gwc2
	cp consang.opt consang
	cp gwd.opt gwd
	cp gwu.opt gwu
	cp update_nldb.opt update_nldb
	cp check_base.opt check_base
	$(STRIP) gwc gwc2 consang gwd gwu update_nldb check_base

compilation.ml: always
	echo "Util.compilation_time_hook.val := \" ($$(date "+%Y-%m-%d %T %Z"))\";" > $@

gwlib.ml:
	echo "value prefix =" > $@
	echo "  try Sys.getenv \"GWPREFIX\" with" >> $@
	echo "  [ Not_found -> \"$(PREFIX)\" ]" | sed -e 's|\\|/|g' >> $@
	echo ";" >> $@

gwc1.out: $(GWC_OBJS)
	$(OCAMLC) -custom $(STATIC) unix.cma $(GWC_OBJS) $(LIBUNIX) -o gwc1.out

gwc1.opt: $(GWC_OBJS:.cmo=.cmx)
	$(OCAMLOPT) $(STATIC) unix.cmxa $(GWC_OBJS:.cmo=.cmx) $(LIBUNIX) -o gwc1.opt

gwc2.out: $(GWC2_OBJS)
	$(OCAMLC) -custom $(STATIC) unix.cma $(GWC2_OBJS) $(LIBUNIX) -o gwc2.out

gwc2.opt: $(GWC2_OBJS:.cmo=.cmx)
	$(OCAMLOPT) $(STATIC) unix.cmxa $(GWC2_OBJS:.cmo=.cmx) $(LIBUNIX) -o gwc2.opt

consang.out: $(CONSANG_OBJS)
	$(OCAMLC) -custom $(STATIC) unix.cma $(LIBUNIX) $(CONSANG_OBJS) -o consang.out

consang.opt: $(CONSANG_OBJS:.cmo=.cmx)
	$(OCAMLOPT) $(STATIC) unix.cmxa $(LIBUNIX) $(CONSANG_OBJS:.cmo=.cmx) -o consang.opt

link.opt:
	$(OCAMLOPT) -I ../api/service -c ../api/service/api_link_tree_piqi.ml
	$(OCAMLOPT) -I ../api/service -c ../api/service/api_link_tree_piqi_ext.ml
	$(OCAMLOPT) -I ../api/service -package curl -I $(OCURL) curl.cmxa -I ../wserver -c link.ml

gwd.out: $(GWD_OBJS)
	$(OCAMLC) -custom $(STATIC) $(OCAMLD) unix.cma str.cma -I $(CAMLP5D) gramlib.cma $(GWD_OBJS) $(LIBUNIX) -o gwd.out

gwd.opt: $(GWD_OBJS:.cmo=.cmx)
ifeq ($(WITH_API),y)
	$(OCAMLOPT) -linkpkg -package curl -I $(OCURL) curl.cmxa $(STATIC) str.cmxa -I $(CAMLP5D) gramlib.cmxa $^ $(LIBUNIX) -o gwd.opt
else
	$(OCAMLOPT) $(STATIC) unix.cmxa str.cmxa -I $(CAMLP5D) gramlib.cmxa $^ $(LIBUNIX) -o gwd.opt
endif

gwu.out: $(GWU_OBJS)
	$(OCAMLC) -custom $(STATIC) unix.cma $(LIBUNIX) $(GWU_OBJS) -o gwu.out

gwu.opt: $(GWU_OBJS:.cmo=.cmx)
	$(OCAMLOPT) $(STATIC) unix.cmxa $(LIBUNIX) $(GWU_OBJS:.cmo=.cmx) -o gwu.opt

phonygwd.out: $(PHONYGWD_OBJS)
	$(OCAMLC) -custom $(STATIC) unix.cma $(LIBUNIX) $(PHONYGWD_OBJS) -o phonygwd.out

probot.out: $(PROBOT_OBJS)
	$(OCAMLC) -custom $(STATIC) unix.cma $(LIBUNIX) $(PROBOT_OBJS) -o probot.out

mostdesc.out: $(MOSTDESC_OBJS)
	$(OCAMLC) -custom $(STATIC) unix.cma $(LIBUNIX) $(MOSTDESC_OBJS) -o mostdesc.out

mostdesc.opt: $(MOSTDESC_OBJS:.cmo=.cmx)
	$(OCAMLOPT) $(STATIC) unix.cmxa $(LIBUNIX) $(MOSTDESC_OBJS:.cmo=.cmx) -o mostdesc.opt

fpla.out: $(FPLA_OBJS)
	$(OCAMLC) -custom $(STATIC) unix.cma $(LIBUNIX) $(FPLA_OBJS) -o fpla.out

fpla.opt: $(FPLA_OBJS:.cmo=.cmx)
	$(OCAMLOPT) $(STATIC) unix.cmxa $(LIBUNIX) $(FPLA_OBJS:.cmo=.cmx) -o fpla.opt

update_nldb.out: $(UPDATE_NLDB_OBJS)
	$(OCAMLC) -custom $(STATIC) unix.cma $(LIBUNIX) $(UPDATE_NLDB_OBJS) -o update_nldb.out

update_nldb.opt: $(UPDATE_NLDB_OBJS:.cmo=.cmx)
	$(OCAMLOPT) $(STATIC) unix.cmxa $(LIBUNIX) $(UPDATE_NLDB_OBJS:.cmo=.cmx) -o update_nldb.opt

check_base.out: $(CHECK_BASE_OBJS)
	$(OCAMLC) -custom $(STATIC) unix.cma $(LIBUNIX) $(CHECK_BASE_OBJS) -o check_base.out

check_base.opt: $(CHECK_BASE_OBJS:.cmo=.cmx)
	$(OCAMLOPT) $(STATIC) unix.cmxa $(LIBUNIX) $(CHECK_BASE_OBJS:.cmo=.cmx) -o check_base.opt

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

.PHONY: clean
clean:
	$(RM) *.out *.opt *.cm[oixa] *.cmxa *.pp[oi] *.o *.a *.obj *.lib \
	*.lck *.bak *~ .#* *.annot
	$(RM) gwc gwc1 gwc2 consang gwd gwu update_nldb check_base gwlib.ml

always:

.PHONY: depend
depend: pa_lock.cmo pa_html.cmo q_codes.cmo
	export LC_ALL=C; $(ROOT)/tools/camlp5_depend.sh $(CAMLP5F) $(OCAMLI) -- *.ml* > .depend

.depend:
	@if [ ! -e $@ ]; then $(MAKE) -o $@ depend; fi

ifeq ($(filter $(MAKECMDGOALS),clean depend),)
  include .depend
endif
