# $Id: Makefile.oneshot,v 1.0 2013-10-22 12:40:41 flh Exp $

GW=../../src
OBJS=$(GW)/secure.cmo $(GW)/buff.cmo $(GW)/name.cmo $(GW)/iovalue.cmo $(GW)/mutil.cmo $(GW)/db2.cmo $(GW)/adef.cmo $(GW)/db2disk.cmo $(GW)/futil.cmo $(GW)/btree.cmo $(GW)/dutil.cmo $(GW)/database.cmo $(GW)/gwdb.cmo $(GW)/checkItem.cmo $(GW)/gutil.cmo $(GW)/consang.cmo

all: gwCreateDicoPlace

clean:
	rm -f *.o *.cm[iox] gwCreateDicoPlace

gwCreateDicoPlace: gwCreateDicoPlace.cmx
	ocamlopt.opt unix.cmxa $(OBJS:.cmo=.cmx) $< -o $@

%.cmx: %.ml
	ocamlopt.opt -c -pp camlp5r -I $(GW) $<
