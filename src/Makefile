# $Id: Makefile,v 1.9 1998-11-24 00:39:43 ddr Exp $

include ../tools/Makefile.inc

OCAMLI=-I ../wserver
GWC_OBJS=argl.cmo mhashtbl.cmo lock.cmo adef.cmo iovalue.cmo name.cmo ansel.cmo gutil.cmo btree.cmo iobase.cmo check.cmo pqueue.cmo consang.cmo gwcomp.cmo gwc.cmo
CONSANG_OBJS=argl.cmo mhashtbl.cmo lock.cmo adef.cmo iovalue.cmo name.cmo ansel.cmo gutil.cmo btree.cmo iobase.cmo pqueue.cmo consang.cmo mk_consang.cmo
GWD_OBJS=../wserver/wserver.cmo argl.cmo mhashtbl.cmo lock.cmo adef.cmo iovalue.cmo name.cmo ansel.cmo gutil.cmo btree.cmo iobase.cmo pqueue.cmo consang.cmo num.cmo util.cmo date.cmo srcfile.cmo perso.cmo update.cmo updateInd.cmo updateIndOk.cmo updateFam.cmo updateFamOk.cmo merge.cmo mergeFam.cmo mergeFamOk.cmo mergeInd.cmo mergeIndOk.cmo relation.cmo relationLink.cmo ascend.cmo descend.cmo alln.cmo some.cmo advSearchOk.cmo birthday.cmo birth.cmo title.cmo family.cmo base64.cmo gwd.cmo
GWU_OBJS=argl.cmo adef.cmo iovalue.cmo name.cmo ansel.cmo gutil.cmo btree.cmo iobase.cmo select.cmo gwu.cmo
all:: out
PHONYGWD_OBJS=../wserver/wserver.cmo argl.cmo phonygwd.cmo
MOSTDESC_OBJS=adef.cmo name.cmo ansel.cmo gutil.cmo iovalue.cmo btree.cmo iobase.cmo pqueue.cmo consang.cmo argl.cmo num.cmo mostdesc.cmo

out:: def.syn.cmo gwc.out consang.out gwd.out gwu.out phonygwd.out i18n_check.out mostdesc.out
	$(RM) gwc consang gwd gwu
	cp gwc.out gwc
	cp consang.out consang
	cp gwd.out gwd
	cp gwu.out gwu

opt:: def.syn.cmo gwc.opt consang.opt gwd.opt gwu.opt
	$(RM) gwd consang gwd gwu
	cp gwc.opt gwc
	cp consang.opt consang
	cp gwd.opt gwd
	cp gwu.opt gwu

clean::
	$(RM) gwc consang gwd gwu

gwc.out: $(GWC_OBJS)
	$(OCAMLC) -custom unix.cma $(GWC_OBJS) $(LIBUNIX) -o gwc.out

gwc.opt: $(GWC_OBJS:.cmo=.cmx)
	$(OCAMLOPT) unix.cmxa $(GWC_OBJS:.cmo=.cmx) $(LIBUNIX) -o gwc.opt

consang.out: $(CONSANG_OBJS)
	$(OCAMLC) -custom unix.cma $(LIBUNIX) $(CONSANG_OBJS) -o consang.out

consang.opt: $(CONSANG_OBJS:.cmo=.cmx)
	$(OCAMLOPT) unix.cmxa $(LIBUNIX) $(CONSANG_OBJS:.cmo=.cmx) -o consang.opt

gwd.out: $(GWD_OBJS)
	$(OCAMLC) -custom unix.cma $(CAMLP4D)/gramlib.cma $(GWD_OBJS) $(LIBUNIX) -o gwd.out

gwd.opt: $(GWD_OBJS:.cmo=.cmx)
	$(OCAMLOPT) unix.cmxa $(CAMLP4D)/gramlib.cmxa $(GWD_OBJS:.cmo=.cmx) $(LIBUNIX) -o gwd.opt

gwu.out: $(GWU_OBJS)
	$(OCAMLC) -custom unix.cma $(LIBUNIX) $(GWU_OBJS) -o gwu.out

gwu.opt: $(GWU_OBJS:.cmo=.cmx)
	$(OCAMLOPT) unix.cmxa $(LIBUNIX) $(GWU_OBJS:.cmo=.cmx) -o gwu.opt

phonygwd.out: $(PHONYGWD_OBJS)
	$(OCAMLC) -custom unix.cma $(LIBUNIX) $(PHONYGWD_OBJS) -o phonygwd.out

mostdesc.out: $(MOSTDESC_OBJS)
	$(OCAMLC) -custom unix.cma $(LIBUNIX) $(MOSTDESC_OBJS) -o mostdesc.out

i18n_check.out: i18n_check.cmo
	$(OCAMLC) i18n_check.cmo -o i18n_check.out

i18n: pr_transl.cmo always
	if test -f i18n; then mv i18n i18n.bak; fi
	OBJS=`(grep -w -c "transl conf" *.ml;grep -w -c "transl_nth conf" *.ml) | grep -v :0 | sed s/:.*$$//`; for i in $$OBJS; do echo $$i 1>&2; camlp4r pa_ifdef.cmo ./pa_lock.cmo ./pa_html.cmo ./pr_transl.cmo pa_extend.cmo $$i; done | sort | uniq > i18n

always:

iovalue.cmo: q_codes.cmo
iovalue.cmx: q_codes.cmo
gwc.cmo mk_consang.cmo: pa_lock.cmo
gwc.cmx mk_consang.cmx: pa_lock.cmo

ascend.cmo birth.cmo birthday.cmo descend.cmo family.cmo merge.cmo mergeInd.cmo mergeIndOk.cmo mergeFam.cmo mergeFamOk.cmo relationLink.cmo update.cmo updateInd.cmo updateIndOk.cmo updateFam.cmo: pa_html.cmo
ascend.cmx birth.cmo birthday.cmx descend.cmx family.cmx merge.cmx mergeInd.cmx mergeIndOk.cmx mergeFam.cmx mergeFamOk.cmo relationLink.cmx update.cmx updateInd.cmx updateIndOk.cmo updateFam.cmx: pa_html.cmo

depend:
	ocamldep $(OCAMLI) *.ml* > .depend

include .depend
