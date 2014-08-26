# $Id: Makefile.oneshot,v 1.0 2013-10-22 12:40:41 flh Exp $

GW=../../src
OBJS=$(GW)/secure.cmo $(GW)/buff.cmo $(GW)/name.cmo $(GW)/iovalue.cmo $(GW)/mutil.cmo $(GW)/db2.cmo $(GW)/adef.cmo $(GW)/db2disk.cmo $(GW)/futil.cmo $(GW)/btree.cmo $(GW)/dutil.cmo $(GW)/database.cmo $(GW)/gwdb.cmo $(GW)/checkItem.cmo $(GW)/gutil.cmo $(GW)/consang.cmo
OBJS2=$(GW)/secure.cmo $(GW)/buff.cmo $(GW)/name.cmo $(GW)/iovalue.cmo $(GW)/mutil.cmo $(GW)/db2.cmo $(GW)/adef.cmo $(GW)/db2disk.cmo $(GW)/futil.cmo $(GW)/btree.cmo $(GW)/dutil.cmo $(GW)/database.cmo $(GW)/gwdb.cmo $(GW)/checkItem.cmo $(GW)/gutil.cmo $(GW)/consang.cmo
OBJS_TEXT=$(GW)/secure.cmo $(GW)/buff.cmo $(GW)/name.cmo $(GW)/iovalue.cmo $(GW)/mutil.cmo $(GW)/db2.cmo $(GW)/adef.cmo $(GW)/db2disk.cmo $(GW)/futil.cmo $(GW)/btree.cmo $(GW)/dutil.cmo $(GW)/database.cmo $(GW)/gwdb.cmo $(GW)/checkItem.cmo $(GW)/gutil.cmo $(GW)/consang.cmo
OBJS_EXPORT=$(GW)/secure.cmo $(GW)/buff.cmo $(GW)/name.cmo $(GW)/iovalue.cmo $(GW)/mutil.cmo $(GW)/db2.cmo $(GW)/adef.cmo $(GW)/db2disk.cmo $(GW)/futil.cmo $(GW)/btree.cmo $(GW)/dutil.cmo $(GW)/database.cmo $(GW)/gwdb.cmo $(GW)/checkItem.cmo $(GW)/gutil.cmo $(GW)/consang.cmo

all: gwRemoveImgGallery gwBaseCompatiblePlus gwFixDateText gwExportAscCSV

clean:
	rm -f *.o *.cm[iox] gwRemoveImgGallery gwBaseCompatiblePlus gwFixDateText gwExportAscCSV

gwRemoveImgGallery: gwRemoveImgGallery.cmx
	ocamlopt.opt unix.cmxa $(OBJS:.cmo=.cmx) $< -o $@

gwBaseCompatiblePlus: gwBaseCompatiblePlus.cmx
	ocamlopt.opt unix.cmxa $(OBJS2:.cmo=.cmx) $< -o $@

gwFixDateText: gwFixDateText.cmx
	ocamlopt.opt unix.cmxa $(OBJS_TEXT:.cmo=.cmx) $< -o $@

gwExportAscCSV: gwExportAscCSV.cmx
	ocamlopt.opt unix.cmxa $(OBJS_EXPORT:.cmo=.cmx) $< -o $@

%.cmx: %.ml
	ocamlopt.opt -annot -c -pp camlp5r -I $(GW) $<
