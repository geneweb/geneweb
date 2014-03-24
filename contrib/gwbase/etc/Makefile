# $Id: Makefile,v 4.36 2009-03-11 10:58:50 deraugla Exp $

include Makefile.inc

GWB=../../../
OCAMLI=-I $(GWB)/src
STD_OBJS=$(GWB)/src/adef.cmo $(GWB)/src/buff.cmo $(GWB)/src/name.cmo $(GWB)/src/iovalue.cmo $(GWB)/src/mutil.cmo $(GWB)/src/futil.cmo $(GWB)/src/dutil.cmo $(GWB)/src/btree.cmo $(GWB)/src/secure.cmo $(GWB)/src/database.cmo $(GWB)/src/db2.cmo $(GWB)/src/db2disk.cmo $(GWB)/src/diff.cmo $(GWB)/src/gwdb.cmo $(GWB)/src/checkItem.cmo $(GWB)/src/gutil.cmo
GENEANET_OBJS=$(GWB)/src/adef.cmo $(GWB)/src/iovalue.cmo $(GWB)/src/buff.cmo $(GWB)/src/name.cmo $(GWB)/src/ansel.cmo $(GWB)/src/mutil.cmo $(GWB)/src/futil.cmo $(GWB)/src/dutil.cmo $(GWB)/src/btree.cmo $(GWB)/src/secure.cmo $(GWB)/src/database.cmo $(GWB)/src/db2.cmo $(GWB)/src/db2disk.cmo $(GWB)/src/diff.cmo $(GWB)/src/gwdb.cmo $(GWB)/src/checkItem.cmo $(GWB)/src/gutil.cmo $(GWB)/src/argl.cmo geneanet.cmo
CLAVIER_OBJS=clavier.cmo
CONNEX_OBJS=$(GWB)/src/argl.cmo $(GWB)/src/adef.cmo $(GWB)/src/buff.cmo $(GWB)/src/name.cmo $(GWB)/src/ansel.cmo $(GWB)/src/iovalue.cmo $(GWB)/src/mutil.cmo $(GWB)/src/futil.cmo $(GWB)/src/dutil.cmo $(GWB)/src/btree.cmo $(GWB)/src/secure.cmo $(GWB)/src/database.cmo $(GWB)/src/db2.cmo $(GWB)/src/db2disk.cmo $(GWB)/src/diff.cmo $(GWB)/src/gwdb.cmo $(GWB)/src/checkItem.cmo $(GWB)/src/gutil.cmo $(GWB)/src/lock.cmo connex.cmo
HIST_OBJS=$(GWB)/src/argl.cmo $(GWB)/src/adef.cmo $(GWB)/src/buff.cmo $(GWB)/src/name.cmo $(GWB)/src/iovalue.cmo $(GWB)/src/mutil.cmo $(GWB)/src/futil.cmo $(GWB)/src/dutil.cmo $(GWB)/src/btree.cmo $(GWB)/src/secure.cmo $(GWB)/src/database.cmo $(GWB)/src/db2.cmo $(GWB)/src/db2disk.cmo $(GWB)/src/diff.cmo $(GWB)/src/gwdb.cmo $(GWB)/src/checkItem.cmo $(GWB)/src/gutil.cmo hist.cmo
SELROY_OBJS=$(GWB)/src/argl.cmo $(GWB)/src/adef.cmo $(GWB)/src/iovalue.cmo $(GWB)/src/buff.cmo $(GWB)/src/name.cmo $(GWB)/src/mutil.cmo $(GWB)/src/futil.cmo $(GWB)/src/dutil.cmo $(GWB)/src/btree.cmo $(GWB)/src/secure.cmo $(GWB)/src/database.cmo $(GWB)/src/db2.cmo $(GWB)/src/db2disk.cmo $(GWB)/src/diff.cmo $(GWB)/src/gwdb.cmo $(GWB)/src/checkItem.cmo $(GWB)/src/gutil.cmo select.cmo $(GWB)/src/calendar.cmo selroy.cmo
CHKIMG_OBJS=$(GWB)/src/argl.cmo $(GWB)/src/adef.cmo $(GWB)/src/buff.cmo $(GWB)/src/name.cmo $(GWB)/src/iovalue.cmo $(GWB)/src/mutil.cmo $(GWB)/src/futil.cmo $(GWB)/src/dutil.cmo $(GWB)/src/btree.cmo $(GWB)/src/secure.cmo $(GWB)/src/database.cmo $(GWB)/src/db2.cmo $(GWB)/src/db2disk.cmo $(GWB)/src/diff.cmo $(GWB)/src/gwdb.cmo $(GWB)/src/checkItem.cmo $(GWB)/src/gutil.cmo chkimg.cmo
CONSMOY_OBJS=$(GWB)/src/argl.cmo $(GWB)/src/adef.cmo $(GWB)/src/buff.cmo $(GWB)/src/name.cmo $(GWB)/src/iovalue.cmo $(GWB/src/mutil.cmo $(GWB)/src/futil.cmo $(GWB)/src/dutil.cmo $(GWB)/src/btree.cmo $(GWB)/src/secure.cmo $(GWB)/src/database.cmo $(GWB)/src/db2.cmo $(GWB)/src/diff.cmo $(GWB)/src/gwdb.cmo $(GWB)/src/checkItem.cmo $(GWB)/src/gutil.cmo consmoy.cmo
CLEOPATRE_OBJS=$(STD_OBJS) gwlib.cmo cleopatre.cmo
POPULE_OBJS=$(STD_OBJS) gwlib.cmo $(GWB)/src/calendar.cmo $(GWB)/src/outbase.cmo popule.cmo
MODIF_OBJS=$(STD_OBJS) modif.cmo
PUBLIC_OBJS=$(STD_OBJS) public.cmo
PUBLIC2_OBJS=$(STD_OBJS) $(GWB)/src/progrBar.cmo public2.cmo
DONADELLO_OBJS=$(STD_OBJS) donadello.cmo
PARTICULE_OBJS=$(STD_OBJS) particule.cmo
LUNE_OBJS=$(STD_OBJS) $(GWB)/src/calendar.cmo lune.cmo
TITRES_OBJS=$(STD_OBJS) titres.cmo
GWCK_OBJS=$(STD_OBJS) $(GWB)/src/lock.cmo $(GWB)/src/progrBar.cmo gwck.cmo
NBDESC_OBJS=$(STD_OBJS) $(GWB)/src/progrBar.cmo nbdesc.cmo

all:: opt

out:: connex.out hist.out selroy.out chkimg.out consmoy.out cleopatre.out lune.out popule.out public.out public2.out titres.out gwck.out

opt:: connex.opt hist.opt selroy.opt chkimg.opt consmoy.opt lune.opt popule.opt public.opt public2.opt titres.opt gwck.opt nbdesc.opt

odif.opt: $(MODIF_OBJS:.cmo=.cmx)
	$(OCAMLOPT) unix.cmxa $(LIBUNIX) $(MODIF_OBJS:.cmo=.cmx) -o modif.opt

titres.out: $(TITRES_OBJS)
	$(OCAMLC) unix.cma $(LIBUNIX) $(TITRES_OBJS) -o titres.out

titres.opt: $(TITRES_OBJS:.cmo=.cmx)
	$(OCAMLOPT) unix.cmxa $(LIBUNIX) $(TITRES_OBJS:.cmo=.cmx) -o titres.opt

public.out: $(PUBLIC_OBJS)
	$(OCAMLC) unix.cma $(LIBUNIX) $(PUBLIC_OBJS) -o public.out

public.opt: $(PUBLIC_OBJS:.cmo=.cmx)
	$(OCAMLOPT) unix.cmxa $(LIBUNIX) $(PUBLIC_OBJS:.cmo=.cmx) -o public.opt

public2.opt: $(PUBLIC2_OBJS:.cmo=.cmx)
	$(OCAMLOPT) unix.cmxa $(LIBUNIX) $(PUBLIC2_OBJS:.cmo=.cmx) -o public2.opt

particule.out: $(PARTICULE_OBJS)
	$(OCAMLC) -custom unix.cma $(LIBUNIX) $(PARTICULE_OBJS) -o particule.out

donadello.out: $(DONADELLO_OBJS)
	$(OCAMLC) -custom unix.cma $(LIBUNIX) $(DONADELLO_OBJS) -o donadello.out

donadello.opt: $(DONADELLO_OBJS:.cmo=.cmx)
	$(OCAMLOPT) unix.cmxa $(LIBUNIX) $(DONADELLO_OBJS:.cmo=.cmx) -o donadello.opt

cleopatre.out: $(CLEOPATRE_OBJS)
	$(OCAMLC) -custom unix.cma $(LIBUNIX) $(CLEOPATRE_OBJS) -o cleopatre.out

lune.out: $(LUNE_OBJS)
	$(OCAMLC) unix.cma $(LIBUNIX) $(LUNE_OBJS) -o lune.out

lune.opt: $(LUNE_OBJS:.cmo=.cmx)
	$(OCAMLOPT) unix.cmxa $(LIBUNIX) $(LUNE_OBJS:.cmo=.cmx) -o lune.opt

popule.out: $(POPULE_OBJS)
	$(OCAMLC) unix.cma $(LIBUNIX) $(POPULE_OBJS) -o popule.out

popule.opt: $(POPULE_OBJS:.cmo=.cmx)
	$(OCAMLOPT) unix.cmxa $(LIBUNIX) $(POPULE_OBJS:.cmo=.cmx) -o popule.opt

geneanet.out: $(GENEANET_OBJS)
	$(OCAMLC) -custom unix.cma $(LIBUNIX) $(GENEANET_OBJS) -o geneanet.out

connex.out: $(CONNEX_OBJS)
	$(OCAMLC) -custom unix.cma $(LIBUNIX) $(CONNEX_OBJS) -o connex.out

connex.opt: $(CONNEX_OBJS:.cmo=.cmx)
	$(OCAMLOPT) unix.cmxa $(LIBUNIX) $(CONNEX_OBJS:.cmo=.cmx) -o connex.opt

hist.out: $(HIST_OBJS)
	$(OCAMLC) -custom unix.cma $(LIBUNIX) $(HIST_OBJS) -o hist.out

hist.opt: $(HIST_OBJS:.cmo=.cmx)
	$(OCAMLOPT) unix.cmxa $(LIBUNIX) $(HIST_OBJS:.cmo=.cmx) -o hist.opt

selroy.opt: $(SELROY_OBJS:.cmo=.cmx)
	$(OCAMLOPT) unix.cmxa $(SELROY_OBJS:.cmo=.cmx) $(LIBUNIX) -o selroy.opt

selroy.out: $(SELROY_OBJS)
	$(OCAMLC) -custom unix.cma $(LIBUNIX) $(SELROY_OBJS) -o selroy.out

chkimg.out: $(CHKIMG_OBJS)
	$(OCAMLC) unix.cma $(CHKIMG_OBJS) $(LIBUNIX) -o chkimg.out

chkimg.opt: $(CHKIMG_OBJS:.cmo=.cmx)
	$(OCAMLOPT) unix.cmxa $(CHKIMG_OBJS:.cmo=.cmx) $(LIBUNIX) -o chkimg.opt

consmoy.out: $(CONSMOY_OBJS)
	$(OCAMLC) unix.cma $(CONSMOY_OBJS) $(LIBUNIX) -o consmoy.out

consmoy.opt: $(CONSMOY_OBJS:.cmo=.cmx)
	$(OCAMLOPT) unix.cmxa $(CONSMOY_OBJS:.cmo=.cmx) $(LIBUNIX) -o consmoy.opt

gwck.out: $(GWCK_OBJS)
	$(OCAMLC) -custom $(STATIC) unix.cma $(LIBUNIX) $(GWCK_OBJS) -o gwck.out

gwck.opt: $(GWCK_OBJS:.cmo=.cmx)
	$(OCAMLOPT) $(STATIC) unix.cmxa $(LIBUNIX) $(GWCK_OBJS:.cmo=.cmx) -o gwck.opt

nbdesc.opt: $(NBDESC_OBJS:.cmo=.cmx)
	$(OCAMLOPT) $(STATIC) unix.cmxa $(LIBUNIX) $(NBDESC_OBJS:.cmo=.cmx) -o nbdesc.opt

clavier.out: $(CLAVIER_OBJS)
	$(OCAMLC) -custom $(CLAVIER_OBJS) -o clavier.out

depend:
	export LC_ALL=C; TOP=$(GWB) $(GWB)/tools/camlp5_depend.sh $(OCAMLI) $$(ls *.ml) | sed -e 's|$(GWB)|$$(GWB)|g' > .depend.new
	mv .depend.new .depend

include .depend
