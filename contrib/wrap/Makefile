# $Id: Makefile.gwpublic,v 1.4 2008/03/31 11:36:28 deraugla Exp $


CAMLP5D=~/.opam/system/lib/camlp5/
LIBUNIX=-cclib -lunix

GW=../../src

CONSANG_OBJS=$(GW)/argl.cmo $(GW)/lock.cmo $(GW)/adef.cmo $(GW)/iovalue.cmo $(GW)/buff.cmo $(GW)/name.cmo $(GW)/mutil.cmo $(GW)/secure.cmo $(GW)/btree.cmo $(GW)/futil.cmo $(GW)/dutil.cmo $(GW)/progrBar.cmo $(GW)/database.cmo $(GW)/db2.cmo $(GW)/db2disk.cmo $(GW)/db2out.cmo $(GW)/diff.cmo $(GW)/outbase.cmo $(GW)/gwdb.cmo $(GW)/pqueue.cmo

GED2GWB_OBJS=$(GW)/argl.cmo $(GW)/adef.cmo $(GW)/iovalue.cmo $(GW)/buff.cmo $(GW)/name.cmo $(GW)/ansel.cmo $(GW)/mutil.cmo $(GW)/futil.cmo $(GW)/dutil.cmo $(GW)/secure.cmo $(GW)/btree.cmo $(GW)/database.cmo $(GW)/db2.cmo $(GW)/db2disk.cmo $(GW)/diff.cmo $(GW)/outbase.cmo $(GW)/gwdb.cmo $(GW)/checkItem.cmo $(GW)/calendar.cmo $(GW)/lock.cmo $(GW)/progrBar.cmo $(GW)/consang.cmo $(GW)/consangAll.cmo $(GW)/check.cmo

GED2GWB2_OBJS=$(GW)/buff.cmo $(GW)/argl.cmo $(GW)/secure.cmo $(GW)/adef.cmo $(GW)/iovalue.cmo $(GW)/name.cmo $(GW)/mutil.cmo $(GW)/ansel.cmo $(GW)/calendar.cmo $(GW)/iochan.cmo $(GW)/db2.cmo $(GW)/progrBar.cmo $(GW)/futil.cmo $(GW)/notesLinks.cmo $(GW)/db2out.cmo $(GW)/db2disk.cmo $(GW)/dutil.cmo $(GW)/btree.cmo $(GW)/database.cmo $(GW)/gwdb.cmo $(GW)/consang.cmo $(GW)/consangAll.cmo $(GW)/checkItem.cmo $(GW)/check.cmo $(GW)/db2link.cmo $(GW)/lock.cmo

GWB2GED_OBJS=$(GW)/argl.cmo $(GW)/adef.cmo $(GW)/buff.cmo $(GW)/name.cmo $(GW)/ansel.cmo $(GW)/iovalue.cmo $(GW)/mutil.cmo $(GW)/futil.cmo $(GW)/dutil.cmo $(GW)/secure.cmo $(GW)/btree.cmo $(GW)/database.cmo $(GW)/db2.cmo $(GW)/db2disk.cmo $(GW)/diff.cmo $(GW)/gwdb.cmo $(GW)/checkItem.cmo $(GW)/gutil.cmo $(GW)/select.cmo $(GW)/version.cmo $(GW)/calendar.cmo

GWC1_OBJS=$(GW)/argl.cmo $(GW)/lock.cmo $(GW)/adef.cmo $(GW)/iovalue.cmo $(GW)/buff.cmo $(GW)/name.cmo $(GW)/mutil.cmo $(GW)/secure.cmo $(GW)/btree.cmo $(GW)/futil.cmo $(GW)/dutil.cmo $(GW)/database.cmo $(GW)/db2.cmo $(GW)/db2disk.cmo $(GW)/diff.cmo $(GW)/outbase.cmo $(GW)/gwdb.cmo $(GW)/pqueue.cmo $(GW)/progrBar.cmo $(GW)/consang.cmo $(GW)/consangAll.cmo $(GW)/checkItem.cmo $(GW)/check.cmo $(GW)/calendar.cmo $(GW)/notesLinks.cmo $(GW)/gwcomp.cmo $(GW)/db1link.cmo

GWC2_OBJS=$(GW)/calendar.cmo $(GW)/adef.cmo $(GW)/iovalue.cmo $(GW)/buff.cmo $(GW)/name.cmo $(GW)/mutil.cmo $(GW)/gwcomp.cmo $(GW)/argl.cmo $(GW)/lock.cmo $(GW)/progrBar.cmo $(GW)/secure.cmo $(GW)/iochan.cmo $(GW)/futil.cmo $(GW)/db2.cmo $(GW)/db2out.cmo $(GW)/notesLinks.cmo $(GW)/db2disk.cmo $(GW)/dutil.cmo $(GW)/btree.cmo $(GW)/database.cmo $(GW)/gwdb.cmo $(GW)/consang.cmo $(GW)/consangAll.cmo $(GW)/checkItem.cmo $(GW)/check.cmo $(GW)/db2link.cmo

GWPUBLIC_OBJS=$(GW)/secure.cmo $(GW)/buff.cmo $(GW)/name.cmo $(GW)/iovalue.cmo $(GW)/mutil.cmo $(GW)/db2.cmo $(GW)/adef.cmo $(GW)/db2disk.cmo $(GW)/futil.cmo $(GW)/btree.cmo $(GW)/dutil.cmo $(GW)/database.cmo $(GW)/gwdb.cmo $(GW)/checkItem.cmo $(GW)/gutil.cmo $(GW)/consang.cmo

GWU_OBJS=$(GW)/argl.cmo $(GW)/adef.cmo $(GW)/iovalue.cmo $(GW)/buff.cmo $(GW)/name.cmo $(GW)/mutil.cmo $(GW)/secure.cmo $(GW)/btree.cmo $(GW)/futil.cmo $(GW)/dutil.cmo $(GW)/database.cmo $(GW)/db2.cmo $(GW)/db2disk.cmo $(GW)/diff.cmo $(GW)/gwdb.cmo $(GW)/checkItem.cmo $(GW)/gutil.cmo $(GW)/select.cmo $(GW)/calendar.cmo $(GW)/notesLinks.cmo $(GW)/progrBar.cmo

UPDATE_NLDB_OBJS=$(GW)/secure.cmo $(GW)/buff.cmo $(GW)/name.cmo $(GW)/adef.cmo $(GW)/iovalue.cmo $(GW)/mutil.cmo $(GW)/btree.cmo $(GW)/futil.cmo $(GW)/dutil.cmo $(GW)/database.cmo $(GW)/db2.cmo $(GW)/db2disk.cmo $(GW)/diff.cmo $(GW)/gwdb.cmo $(GW)/checkItem.cmo $(GW)/gutil.cmo $(GW)/argl.cmo $(GW)/notesLinks.cmo $(GW)/progrBar.cmo


all: consang ged2gwb2 ged2gwb gwb2ged gwc1 gwc2 gwiftitles gwprivate gwpublic1 gwpublic2 gwpublic2priv gwu update_nldb

clean:
	rm -f *.o *.cm[iox] consang ged2gwb2 ged2gwb gwb2ged gwc gwc1 gwc2 gwiftitles gwprivate gwpublic1 gwpublic2 gwpublic2priv gwu update_nldb

consang: consang.cmx
	ocamlopt.opt unix.cmxa $(CONSANG_OBJS:.cmo=.cmx) $< -o $@

ged2gwb: ged2gwb.cmx
	ocamlopt.opt unix.cmxa str.cmxa $(LIBUNIX) -I $(CAMLP5D) gramlib.cmxa $(GED2GWB_OBJS:.cmo=.cmx) $< -o $@

ged2gwb2: ged2gwb2.cmx
	ocamlopt.opt unix.cmxa str.cmxa $(LIBUNIX) -I $(CAMLP5D) gramlib.cmxa $(GED2GWB2_OBJS:.cmo=.cmx) $< -o $@

gwb2ged: gwb2ged.cmx
	ocamlopt.opt unix.cmxa $(GWB2GED_OBJS:.cmo=.cmx) $< -o $@

gwc1: gwc1.cmx
	ocamlopt.opt unix.cmxa $(GWC1_OBJS:.cmo=.cmx) $< -o $@

gwc2: gwc2.cmx
	ocamlopt.opt unix.cmxa $(GWC2_OBJS:.cmo=.cmx) $< -o $@

gwiftitles: gwiftitles.cmx
	ocamlopt.opt unix.cmxa $(GWPUBLIC_OBJS:.cmo=.cmx) $< -o $@

gwprivate: gwprivate.cmx
	ocamlopt.opt unix.cmxa $(GWPUBLIC_OBJS:.cmo=.cmx) $< -o $@

gwpublic1: gwpublic1.cmx
	ocamlopt.opt unix.cmxa $(GWPUBLIC_OBJS:.cmo=.cmx) $< -o $@

gwpublic2: gwpublic2.cmx
	ocamlopt.opt unix.cmxa $(GWPUBLIC_OBJS:.cmo=.cmx) $< -o $@

gwpublic2priv: gwpublic2priv.cmx
	ocamlopt.opt unix.cmxa $(GWPUBLIC_OBJS:.cmo=.cmx) $< -o $@

gwu: gwu.cmx
	ocamlopt.opt unix.cmxa $(GWU_OBJS:.cmo=.cmx) $< -o $@

update_nldb: update_nldb.cmx
	ocamlopt.opt unix.cmxa $(UPDATE_NLDB_OBJS:.cmo=.cmx) $< -o $@

%.cmx: %.ml
	ocamlopt.opt -c -pp camlp5r -I $(GW) $<
