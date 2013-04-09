# $Id: Makefile.gwpublic,v 1.4 2008/03/31 11:36:28 deraugla Exp $

GW=../../src
OBJS=$(GW)/secure.cmo $(GW)/buff.cmo $(GW)/name.cmo $(GW)/iovalue.cmo $(GW)/mutil.cmo $(GW)/db2.cmo $(GW)/adef.cmo $(GW)/db2disk.cmo $(GW)/futil.cmo $(GW)/btree.cmo $(GW)/dutil.cmo $(GW)/database.cmo $(GW)/gwdb.cmo $(GW)/checkItem.cmo $(GW)/gutil.cmo $(GW)/consang.cmo

all: gwpublic1 gwpublic2 gwpublic2priv gwprivate gwiftitles

clean:
	rm -f *.o *.cm[iox] gwpublic1 gwpublic2 gwpublic2priv gwprivate gwiftitles

gwpublic1: gwpublic1.cmx
	ocamlopt.opt unix.cmxa $(OBJS:.cmo=.cmx) $< -o $@

gwpublic2: gwpublic2.cmx
	ocamlopt.opt unix.cmxa $(OBJS:.cmo=.cmx) $(GW)/progrBar.cmx $< -o $@

gwpublic2priv: gwpublic2priv.cmx
	ocamlopt.opt unix.cmxa $(OBJS:.cmo=.cmx) $(GW)/progrBar.cmx $< -o $@

gwprivate: gwprivate.cmx
	ocamlopt.opt unix.cmxa $(OBJS:.cmo=.cmx) $< -o $@

gwiftitles: gwiftitles.cmx
	ocamlopt.opt unix.cmxa $(OBJS:.cmo=.cmx) $< -o $@

%.cmx: %.ml
	ocamlopt.opt -c -pp camlp5r -I $(GW) $<
