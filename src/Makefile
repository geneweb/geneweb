# $Id: Makefile,v 2.5 1999-04-15 10:38:27 ddr Exp $

include ../tools/Makefile.inc

OCAMLI=-I ../wserver
GWC_OBJS=argl.cmo mhashtbl.cmo lock.cmo adef.cmo iovalue.cmo name.cmo ansel.cmo gutil.cmo btree.cmo iobase.cmo check.cmo pqueue.cmo consang.cmo gwcomp.cmo gwc.cmo
CONSANG_OBJS=argl.cmo mhashtbl.cmo lock.cmo adef.cmo iovalue.cmo name.cmo ansel.cmo gutil.cmo btree.cmo iobase.cmo pqueue.cmo consang.cmo mk_consang.cmo
GWD_OBJS1=../wserver/wserver.cmo argl.cmo mhashtbl.cmo lock.cmo adef.cmo iovalue.cmo name.cmo ansel.cmo gutil.cmo btree.cmo iobase.cmo pqueue.cmo consang.cmo num.cmo util.cmo date.cmo srcfile.cmo perso.cmo update.cmo updateInd.cmo updateIndOk.cmo updateFam.cmo updateFamOk.cmo
GWD_OBJS2=merge.cmo mergeFamOk.cmo mergeFam.cmo mergeInd.cmo mergeIndOk.cmo sendImage.cmo relation.cmo relationLink.cmo ascend.cmo descend.cmo cousins.cmo alln.cmo some.cmo advSearchOk.cmo birthday.cmo birthDeath.cmo title.cmo family.cmo base64.cmo gwd.cmo
GWD_OBJS=$(GWD_OBJS1) $(GWD_OBJS2)
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

i18n_check.out: i18n_check.cmo
	$(OCAMLC) i18n_check.cmo -o i18n_check.out

i18n: pr_transl.cmo always
	if test -f i18n; then mv i18n i18n.bak; fi
	OBJS=`(grep -w -c "transl conf" *.ml;grep -w -c "transl_nth conf" *.ml) | grep -v :0 | sed s/:.*$$//`; for i in $$OBJS; do echo $$i 1>&2; camlp4r pa_ifdef.cmo ./pa_lock.cmo ./pa_html.cmo ./pr_transl.cmo pa_extend.cmo $$i; done | sort | uniq > i18n

always:

depend:
	../tools/camlp4_depend.sh $(OCAMLI) *.ml* > .depend.new
	mv .depend.new .depend

include .depend
