# $Id: Makefile.history,v 1.00 2013/09/05 10:09:42 flh Exp $

GW=../../src
OBJS=$(GW)/secure.cmo

all: convert_hist fix_hist is_gw_plus

clean:
	rm -f *.o *.cm[iox] convert_hist fix_hist is_gw_plus

convert_hist: convert_hist.cmx
	ocamlopt.opt unix.cmxa $(OBJS:.cmo=.cmx) $(GW)/progrBar.cmx $< -o $@

is_gw_plus: is_gw_plus.cmx
	ocamlopt.opt unix.cmxa $(OBJS:.cmo=.cmx) $(GW)/progrBar.cmx $< -o $@

fix_hist: fix_hist.cmx
	ocamlopt.opt unix.cmxa $(OBJS:.cmo=.cmx) $(GW)/progrBar.cmx $< -o $@

%.cmx: %.ml
	ocamlopt.opt -g -annot -c -pp camlp5r -I $(GW) $<
