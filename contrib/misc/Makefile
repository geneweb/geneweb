# $Id: Makefile.history,v 1.00 2013/09/05 10:09:42 flh Exp $

GW=../../src
OBJS_STR=$(GW)/secure.cmo $(GW)/buff.cmo $(GW)/name.cmo $(GW)/iovalue.cmo $(GW)/mutil.cmo $(GW)/db2.cmo $(GW)/adef.cmo $(GW)/db2disk.cmo $(GW)/futil.cmo $(GW)/btree.cmo $(GW)/dutil.cmo $(GW)/database.cmo $(GW)/gwdb.cmo $(GW)/checkItem.cmo $(GW)/gutil.cmo $(GW)/consang.cmo

all: lower_string

clean:
	rm -f *.o *.cm[iox] *.annot lower_string

lower_string: lower_string.cmx
	ocamlopt.opt unix.cmxa $(OBJS_STR:.cmo=.cmx) $< -o $@

%.cmx: %.ml
	ocamlopt.opt -g -annot -c -pp camlp5r -I $(GW) $<
