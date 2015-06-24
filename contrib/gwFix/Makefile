# $Id: Makefile,v 0.001 2014-02-21 16:47:52 flh Exp $

include ../../tools/Makefile.inc

TOP=../..
CAMLP5_COMM=TOP=$(TOP) $(TOP)/tools/camlp5_comm.sh

GW=../..
OCAMLI=-I $(GW)/src
OBJS=$(GW)/src/argl.cmo $(GW)/src/adef.cmo $(GW)/src/iovalue.cmo $(GW)/src/buff.cmo $(GW)/src/name.cmo $(GW)/src/ansel.cmo $(GW)/src/mutil.cmo $(GW)/src/futil.cmo $(GW)/src/dutil.cmo $(GW)/src/secure.cmo $(GW)/src/btree.cmo $(GW)/src/database.cmo $(GW)/src/db2.cmo $(GW)/src/db2disk.cmo $(GW)/src/diff.cmo $(GW)/src/outbase.cmo $(GW)/src/gwdb.cmo $(GW)/src/checkItem.cmo $(GW)/src/calendar.cmo $(GW)/src/lock.cmo $(GW)/src/progrBar.cmo $(GW)/src/consang.cmo $(GW)/src/consangAll.cmo $(GW)/src/check.cmo gwFixBase.cmo
OBJS_FIX_FILE=$(GW)/src/argl.cmo $(GW)/src/adef.cmo $(GW)/src/iovalue.cmo $(GW)/src/buff.cmo $(GW)/src/name.cmo $(GW)/src/ansel.cmo $(GW)/src/mutil.cmo $(GW)/src/futil.cmo $(GW)/src/dutil.cmo $(GW)/src/secure.cmo $(GW)/src/btree.cmo $(GW)/src/database.cmo $(GW)/src/db2.cmo $(GW)/src/db2disk.cmo $(GW)/src/diff.cmo $(GW)/src/outbase.cmo $(GW)/src/gwdb.cmo $(GW)/src/checkItem.cmo $(GW)/src/gutil.cmo $(GW)/src/calendar.cmo $(GW)/src/lock.cmo $(GW)/src/progrBar.cmo $(GW)/src/consang.cmo $(GW)/src/consangAll.cmo $(GW)/src/check.cmo gwFixFromFile.cmo
OBJS_FIX_FILE_DOMICILE=$(GW)/src/argl.cmo $(GW)/src/adef.cmo $(GW)/src/iovalue.cmo $(GW)/src/buff.cmo $(GW)/src/name.cmo $(GW)/src/ansel.cmo $(GW)/src/mutil.cmo $(GW)/src/futil.cmo $(GW)/src/dutil.cmo $(GW)/src/secure.cmo $(GW)/src/btree.cmo $(GW)/src/database.cmo $(GW)/src/db2.cmo $(GW)/src/db2disk.cmo $(GW)/src/diff.cmo $(GW)/src/outbase.cmo $(GW)/src/gwdb.cmo $(GW)/src/checkItem.cmo $(GW)/src/gutil.cmo $(GW)/src/calendar.cmo $(GW)/src/lock.cmo $(GW)/src/progrBar.cmo $(GW)/src/consang.cmo $(GW)/src/consangAll.cmo $(GW)/src/check.cmo gwFixFromFileDomicile.cmo
OBJS_FIX_FILE_ALIAS=$(GW)/src/argl.cmo $(GW)/src/adef.cmo $(GW)/src/iovalue.cmo $(GW)/src/buff.cmo $(GW)/src/name.cmo $(GW)/src/ansel.cmo $(GW)/src/mutil.cmo $(GW)/src/futil.cmo $(GW)/src/dutil.cmo $(GW)/src/secure.cmo $(GW)/src/btree.cmo $(GW)/src/database.cmo $(GW)/src/db2.cmo $(GW)/src/db2disk.cmo $(GW)/src/diff.cmo $(GW)/src/outbase.cmo $(GW)/src/gwdb.cmo $(GW)/src/checkItem.cmo $(GW)/src/gutil.cmo $(GW)/src/calendar.cmo $(GW)/src/lock.cmo $(GW)/src/progrBar.cmo $(GW)/src/consang.cmo $(GW)/src/consangAll.cmo $(GW)/src/check.cmo gwFixFromFileAlias.cmo
OBJS_FIX_BURIAL=$(GW)/src/argl.cmo $(GW)/src/adef.cmo $(GW)/src/iovalue.cmo $(GW)/src/buff.cmo $(GW)/src/name.cmo $(GW)/src/ansel.cmo $(GW)/src/mutil.cmo $(GW)/src/futil.cmo $(GW)/src/dutil.cmo $(GW)/src/secure.cmo $(GW)/src/btree.cmo $(GW)/src/database.cmo $(GW)/src/db2.cmo $(GW)/src/db2disk.cmo $(GW)/src/diff.cmo $(GW)/src/outbase.cmo $(GW)/src/gwdb.cmo $(GW)/src/checkItem.cmo $(GW)/src/gutil.cmo $(GW)/src/calendar.cmo $(GW)/src/lock.cmo $(GW)/src/progrBar.cmo $(GW)/src/consang.cmo $(GW)/src/consangAll.cmo $(GW)/src/check.cmo gwFixBurial.cmo
OBJS_FIX_EVT_SRC=$(GW)/src/argl.cmo $(GW)/src/adef.cmo $(GW)/src/iovalue.cmo $(GW)/src/buff.cmo $(GW)/src/name.cmo $(GW)/src/ansel.cmo $(GW)/src/mutil.cmo $(GW)/src/futil.cmo $(GW)/src/dutil.cmo $(GW)/src/secure.cmo $(GW)/src/btree.cmo $(GW)/src/database.cmo $(GW)/src/db2.cmo $(GW)/src/db2disk.cmo $(GW)/src/diff.cmo $(GW)/src/outbase.cmo $(GW)/src/gwdb.cmo $(GW)/src/checkItem.cmo $(GW)/src/gutil.cmo $(GW)/src/calendar.cmo $(GW)/src/lock.cmo $(GW)/src/progrBar.cmo $(GW)/src/consang.cmo $(GW)/src/consangAll.cmo $(GW)/src/check.cmo gwFixEvtSrc.cmo
OBJS_FIX_COLON_SRC=$(GW)/src/argl.cmo $(GW)/src/adef.cmo $(GW)/src/iovalue.cmo $(GW)/src/buff.cmo $(GW)/src/name.cmo $(GW)/src/ansel.cmo $(GW)/src/mutil.cmo $(GW)/src/futil.cmo $(GW)/src/dutil.cmo $(GW)/src/secure.cmo $(GW)/src/btree.cmo $(GW)/src/database.cmo $(GW)/src/db2.cmo $(GW)/src/db2disk.cmo $(GW)/src/diff.cmo $(GW)/src/outbase.cmo $(GW)/src/gwdb.cmo $(GW)/src/checkItem.cmo $(GW)/src/gutil.cmo $(GW)/src/calendar.cmo $(GW)/src/lock.cmo $(GW)/src/progrBar.cmo $(GW)/src/consang.cmo $(GW)/src/consangAll.cmo $(GW)/src/check.cmo gwFixColon.cmo
OBJS_CPL=$(GW)/src/argl.cmo $(GW)/src/adef.cmo $(GW)/src/iovalue.cmo $(GW)/src/buff.cmo $(GW)/src/name.cmo $(GW)/src/ansel.cmo $(GW)/src/mutil.cmo $(GW)/src/futil.cmo $(GW)/src/dutil.cmo $(GW)/src/secure.cmo $(GW)/src/btree.cmo $(GW)/src/database.cmo $(GW)/src/db2.cmo $(GW)/src/db2disk.cmo $(GW)/src/diff.cmo $(GW)/src/outbase.cmo $(GW)/src/gwdb.cmo $(GW)/src/checkItem.cmo $(GW)/src/calendar.cmo $(GW)/src/lock.cmo $(GW)/src/progrBar.cmo $(GW)/src/consang.cmo $(GW)/src/consangAll.cmo $(GW)/src/check.cmo gwFindCpl.cmo
OBJS_OCCU=$(GW)/src/argl.cmo $(GW)/src/adef.cmo $(GW)/src/iovalue.cmo $(GW)/src/buff.cmo $(GW)/src/name.cmo $(GW)/src/ansel.cmo $(GW)/src/mutil.cmo $(GW)/src/futil.cmo $(GW)/src/dutil.cmo $(GW)/src/secure.cmo $(GW)/src/btree.cmo $(GW)/src/database.cmo $(GW)/src/db2.cmo $(GW)/src/db2disk.cmo $(GW)/src/diff.cmo $(GW)/src/outbase.cmo $(GW)/src/gwdb.cmo $(GW)/src/checkItem.cmo $(GW)/src/gutil.cmo $(GW)/src/calendar.cmo $(GW)/src/lock.cmo $(GW)/src/progrBar.cmo $(GW)/src/consang.cmo $(GW)/src/consangAll.cmo $(GW)/src/check.cmo gwFixY.cmo

all:: opt

out:: gwFixBase.out gwFixFromFile.out gwFixFromFileDomicile.out gwFixFromFileAlias.out gwFixBurial.out gwFixEvtSrc.out gwFixColon.out gwFindCpl.out gwFixY.out
	$(RM) gwFixBase gwFixFromFile gwFixFromFileDomicile gwFixFromFileAlias gwFixBurial gwFixEvtSrc gwFixColon gwFindCpl gwFixY
	cp gwFixBase.out gwFixBase
	cp gwFixFromFile.out gwFixFromFile
	cp gwFixFromFileDomicile.out gwFixFromFileDomicile
	cp gwFixFromFileAlias.out gwFixFromFileAlias
	cp gwFixBurial.out gwFixBurial
	cp  gwFixEvtSrc.out  gwFixEvtSrc
	cp gwFixColon.out gwFixColon
	cp gwFindCpl.out gwFindCpl
	cp gwFixY.out gwFixY

opt:: gwFixBase.opt gwFixFromFile.opt gwFixFromFileDomicile.opt gwFixFromFileAlias.opt gwFixBurial.opt gwFixEvtSrc.opt gwFixColon.opt gwFindCpl.opt gwFixY.opt
	$(RM) gwFixBase gwFixFromFile gwFixFromFileDomicile gwFixFromFileAlias gwFixBurial gwFixEvtSrc gwFixColon gwFindCpl gwFixY
	cp gwFixBase.opt gwFixBase
	cp gwFixFromFile.opt gwFixFromFile
	cp gwFixFromFileDomicile.opt gwFixFromFileDomicile
	cp gwFixFromFileAlias.opt gwFixFromFileAlias
	cp gwFixBurial.opt gwFixBurial
	cp gwFixEvtSrc.opt gwFixEvtSrc
	cp gwFixColon.opt gwFixColon
	cp gwFindCpl.opt gwFindCpl
	cp gwFixY.opt gwFixY
	$(STRIP) gwFixBase gwFixFromFile gwFixFromFileDomicile gwFixFromFileAlias gwFixBurial gwFixEvtSrc gwFixColon gwFindCpl gwFixY

clean::
	$(RM) gwFixBase gwFixFromFile gwFixFromFileDomicile gwFixFromFileAlias gwFixBurial gwFixEvtSrc gwFixColon gwFindCpl gwFixY

gwFixBase.out: $(OBJS)
	$(OCAMLC) -custom $(STATIC) $(OCAMLD) unix.cma str.cma $(LIBUNIX) -I $(CAMLP5D) gramlib.cma $(OBJS) -o gwFixBase.out

gwFixBase.opt: $(OBJS:.cmo=.cmx)
	$(OCAMLOPT) $(STATIC) unix.cmxa str.cmxa $(LIBUNIX) -I $(CAMLP5D) gramlib.cmxa $(OBJS:.cmo=.cmx) -o gwFixBase.opt

gwFixFromFile.out: $(OBJS_FIX_FILE)
	$(OCAMLC) -custom $(STATIC) $(OCAMLD) unix.cma str.cma $(LIBUNIX) -I $(CAMLP5D) gramlib.cma $(OBJS_FIX_FILE) -o gwFixFromFile.out

gwFixFromFile.opt: $(OBJS_FIX_FILE:.cmo=.cmx)
	$(OCAMLOPT) $(STATIC) unix.cmxa str.cmxa $(LIBUNIX) -I $(CAMLP5D) gramlib.cmxa $(OBJS_FIX_FILE:.cmo=.cmx) -o gwFixFromFile.opt

gwFixFromFileDomicile.out: $(OBJS_FIX_FILE_DOMICILE)
	$(OCAMLC) -custom $(STATIC) $(OCAMLD) unix.cma str.cma $(LIBUNIX) -I $(CAMLP5D) gramlib.cma $(OBJS_FIX_FILE_DOMICILE) -o gwFixFromFileDomicile.out

gwFixFromFileDomicile.opt: $(OBJS_FIX_FILE_DOMICILE:.cmo=.cmx)
	$(OCAMLOPT) $(STATIC) unix.cmxa str.cmxa $(LIBUNIX) -I $(CAMLP5D) gramlib.cmxa $(OBJS_FIX_FILE_DOMICILE:.cmo=.cmx) -o gwFixFromFileDomicile.opt

gwFixFromFileAlias.out: $(OBJS_FIX_FILE_ALIAS)
	$(OCAMLC) -custom $(STATIC) $(OCAMLD) unix.cma str.cma $(LIBUNIX) -I $(CAMLP5D) gramlib.cma $(OBJS_FIX_FILE_ALIAS) -o gwFixFromFileAlias.out

gwFixFromFileAlias.opt: $(OBJS_FIX_FILE_ALIAS:.cmo=.cmx)
	$(OCAMLOPT) $(STATIC) unix.cmxa str.cmxa $(LIBUNIX) -I $(CAMLP5D) gramlib.cmxa $(OBJS_FIX_FILE_ALIAS:.cmo=.cmx) -o gwFixFromFileAlias.opt

gwFixBurial.out: $(OBJS_FIX_BURIAL)
	$(OCAMLC) -custom $(STATIC) $(OCAMLD) unix.cma str.cma $(LIBUNIX) -I $(CAMLP5D) gramlib.cma $(OBJS_FIX_BURIAL) -o gwFixBurial.out

gwFixBurial.opt: $(OBJS_FIX_BURIAL:.cmo=.cmx)
	$(OCAMLOPT) $(STATIC) unix.cmxa str.cmxa $(LIBUNIX) -I $(CAMLP5D) gramlib.cmxa $(OBJS_FIX_BURIAL:.cmo=.cmx) -o gwFixBurial.opt

gwFixEvtSrc.out: $(OBJS_FIX_EVT_SRC)
	$(OCAMLC) -custom $(STATIC) $(OCAMLD) unix.cma str.cma $(LIBUNIX) -I $(CAMLP5D) gramlib.cma $(OBJS_FIX_EVT_SRC) -o gwFixEvtSrc.out

gwFixEvtSrc.opt: $(OBJS_FIX_EVT_SRC:.cmo=.cmx)
	$(OCAMLOPT) $(STATIC) unix.cmxa str.cmxa $(LIBUNIX) -I $(CAMLP5D) gramlib.cmxa $(OBJS_FIX_EVT_SRC:.cmo=.cmx) -o gwFixEvtSrc.opt

gwFixColon.out: $(OBJS_FIX_COLON_SRC)
	$(OCAMLC) -custom $(STATIC) $(OCAMLD) unix.cma str.cma $(LIBUNIX) -I $(CAMLP5D) gramlib.cma $(OBJS_FIX_COLON_SRC) -o gwFixColon.out

gwFixColon.opt: $(OBJS_FIX_COLON_SRC:.cmo=.cmx)
	$(OCAMLOPT) $(STATIC) unix.cmxa str.cmxa $(LIBUNIX) -I $(CAMLP5D) gramlib.cmxa $(OBJS_FIX_COLON_SRC:.cmo=.cmx) -o gwFixColon.opt

gwFindCpl.out: $(OBJS_CPL)
	$(OCAMLC) -custom $(STATIC) $(OCAMLD) unix.cma str.cma $(LIBUNIX) -I $(CAMLP5D) gramlib.cma $(OBJS_CPL) -o gwFindCpl.out

gwFindCpl.opt: $(OBJS_CPL:.cmo=.cmx)
	$(OCAMLOPT) $(STATIC) unix.cmxa str.cmxa $(LIBUNIX) -I $(CAMLP5D) gramlib.cmxa $(OBJS_CPL:.cmo=.cmx) -o gwFindCpl.opt

gwFixY.out: $(OBJS_OCCU)
	$(OCAMLC) -custom $(STATIC) $(OCAMLD) unix.cma str.cma $(LIBUNIX) -I $(CAMLP5D) gramlib.cma $(OBJS_OCCU) -o gwFixY.out

gwFixY.opt: $(OBJS_OCCU:.cmo=.cmx)
	$(OCAMLOPT) $(STATIC) unix.cmxa str.cmxa $(LIBUNIX) -I $(CAMLP5D) gramlib.cmxa $(OBJS_OCCU:.cmo=.cmx) -o gwFixY.opt

depend:
	TOP=.. ../tools/camlp5_depend.sh $(OCAMLI) *.ml* > .depend

include .depend
