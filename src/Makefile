# $Id: Makefile,v 3.12 2000-05-03 09:50:40 ddr Exp $

include ../tools/Makefile.inc

OCAMLI=-I ../wserver
GWC_OBJS=argl.cmo mhashtbl.cmo lock.cmo adef.cmo iovalue.cmo name.cmo buff.cmo gutil.cmo btree.cmo iobase.cmo check.cmo pqueue.cmo consang.cmo consangAll.cmo calendar.cmo gwcomp.cmo gwc.cmo
CONSANG_OBJS=argl.cmo mhashtbl.cmo lock.cmo adef.cmo iovalue.cmo name.cmo buff.cmo gutil.cmo btree.cmo iobase.cmo pqueue.cmo consang.cmo consangAll.cmo mk_consang.cmo
GWD_OBJS1=../wserver/wserver.cmo argl.cmo lock.cmo adef.cmo iovalue.cmo name.cmo buff.cmo gutil.cmo btree.cmo iobase.cmo pqueue.cmo consang.cmo num.cmo version.cmo util.cmo calendar.cmo date.cmo history.cmo srcfile.cmo perso.cmo update.cmo updateInd.cmo updateIndOk.cmo updateFam.cmo updateFamOk.cmo place.cmo
GWD_OBJS2=merge.cmo mergeFamOk.cmo mergeFam.cmo mergeInd.cmo mergeIndOk.cmo changeChildren.cmo sendImage.cmo dag2html.cmo dag.cmo relationLink.cmo relation.cmo ascend.cmo descend.cmo birthday.cmo cousins.cmo alln.cmo some.cmo advSearchOk.cmo birthDeath.cmo title.cmo notes.cmo doc.cmo forum.cmo image.cmo family.cmo base64.cmo robot.cmo
GWD_OBJS=$(GWD_OBJS1) $(GWD_OBJS2) gwd.cmo
GWU_OBJS=argl.cmo adef.cmo iovalue.cmo name.cmo buff.cmo gutil.cmo btree.cmo iobase.cmo select.cmo calendar.cmo gwu.cmo
PHONYGWD_OBJS=../wserver/wserver.cmo argl.cmo phonygwd.cmo
MOSTDESC_OBJS=adef.cmo name.cmo buff.cmo gutil.cmo iovalue.cmo btree.cmo iobase.cmo pqueue.cmo consang.cmo argl.cmo num.cmo mostdesc.cmo
FPLA_OBJS=argl.cmo adef.cmo name.cmo buff.cmo gutil.cmo btree.cmo iovalue.cmo iobase.cmo fpla.cmo

all:: out

out:: def.syn.cmo ansel.cmo gwc.out consang.out gwd.out gwu.out phonygwd.out i18n_check.out mostdesc.out
	$(RM) gwc consang gwd gwu
	cp gwc.out gwc
	cp consang.out consang
	cp gwd.out gwd
	cp gwu.out gwu

opt:: def.syn.cmo ansel.cmx gwc.opt consang.opt gwd.opt gwu.opt
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
	$(OCAMLC) -custom $(OCAMLD) unix.cma $(CAMLP4D)/gramlib.cma $(GWD_OBJS) $(LIBUNIX) -o gwd.out

gwd.opt: $(GWD_OBJS:.cmo=.cmx)
	$(OCAMLOPT) $(GWD_OBJS1:.cmo=.cmx) -a -o x1.cmxa
	$(OCAMLOPT) $(GWD_OBJS2:.cmo=.cmx) -a -o x2.cmxa
	$(OCAMLOPT) unix.cmxa $(CAMLP4D)/gramlib.cmxa x1.cmxa x2.cmxa gwd.cmx $(LIBUNIX) -o gwd.opt

gwu.out: $(GWU_OBJS)
	$(OCAMLC) -custom unix.cma $(LIBUNIX) $(GWU_OBJS) -o gwu.out

gwu.opt: $(GWU_OBJS:.cmo=.cmx)
	$(OCAMLOPT) unix.cmxa $(LIBUNIX) $(GWU_OBJS:.cmo=.cmx) -o gwu.opt

phonygwd.out: $(PHONYGWD_OBJS)
	$(OCAMLC) -custom unix.cma $(LIBUNIX) $(PHONYGWD_OBJS) -o phonygwd.out

mostdesc.out: $(MOSTDESC_OBJS)
	$(OCAMLC) -custom unix.cma $(LIBUNIX) $(MOSTDESC_OBJS) -o mostdesc.out

mostdesc.opt: $(MOSTDESC_OBJS:.cmo=.cmx)
	$(OCAMLOPT) unix.cmxa $(LIBUNIX) $(MOSTDESC_OBJS:.cmo=.cmx) -o mostdesc.opt

fpla.out: $(FPLA_OBJS)
	$(OCAMLC) -custom unix.cma $(LIBUNIX) $(FPLA_OBJS) -o fpla.out

fpla.opt: $(FPLA_OBJS:.cmo=.cmx)
	$(OCAMLOPT) unix.cmxa $(LIBUNIX) $(FPLA_OBJS:.cmo=.cmx) -o fpla.opt

i18n_check.out: i18n_check.cmo
	$(OCAMLC) i18n_check.cmo -o i18n_check.out

i18n: pr_transl.cmo always
	if test -f i18n; then mv i18n i18n.bak; fi
	OBJS=`(grep -w -c "transl conf" *.ml;grep -w -c "transl_nth conf" *.ml) | grep -v :0 | sed s/:.*$$// | sort | uniq`; for i in $$OBJS; do echo $$i 1>&2; camlp4r pa_ifdef.cmo ./pa_lock.cmo ./pa_html.cmo ./pr_transl.cmo pa_extend.cmo $$i; done | sort | uniq > i18n

always:

depend:
	../tools/camlp4_depend.sh $(OCAMLI) *.mli *.ml > .depend.new
	mv .depend.new .depend

include .depend
