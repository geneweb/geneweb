# $Id: Makefile,v 1.1 2001-06-03 14:33:50 ddr Exp $

BINDIR=/usr/local/bin
OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCARGS=
OBJS=dag2html.cmo main.cmo

all: dag2html

out: dag2html.cmo
opt: dag2html.cmx

install:
	cp dag2html $(BINDIR)/.

dag2html: $(OBJS)
	$(OCAMLC) $(OBJS) -o dag2html

clean:
	rm -f dag2html *.cm[oix] *.o

.SUFFIXES: .ml .mli .cmo .cmx .cmi

.ml.cmo:
	camlp4r $< -o $*.ppo -warn_seq
	$(OCAMLC) $(OCARGS) -c -impl $*.ppo
	rm -f $*.ppo

.ml.cmx:
	camlp4r $< -o $*.ppo -warn_seq
	$(OCAMLOPT) $(OCARGS) -c -impl $*.ppo
	rm -f $*.ppo

.mli.cmi:
	camlp4r $< -o $*.ppi
	$(OCAMLC) $(OCARGS) -c -intf $*.ppi
	rm -f $*.ppi

main.cmo: dag2html.cmo
dag2html.cmo: dag2html.cmi
dag2html.cmx: dag2html.cmi
