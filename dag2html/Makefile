# $Id: Makefile,v 5.1 2007-07-11 16:15:21 ddr Exp $

BINDIR=/usr/local/bin
MANDIR=/usr/local/share/man/man1
OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCARGS=
OBJS=dag2html.cmo main.cmo

all: dag2html

out: dag2html.cmo
opt: dag2html.cmx

install:
	cp dag2html $(BINDIR)/.
	cp dag2html.1 $(MANDIR)/.

dag2html: $(OBJS)
	$(OCAMLC) $(OBJS) -o dag2html

clean:
	rm -f dag2html *.cm[oix] *.o

.SUFFIXES: .ml .mli .cmo .cmx .cmi

.ml.cmo:
	camlp5r $< -o $*.ppo
	$(OCAMLC) $(OCARGS) -c -impl $*.ppo
	rm -f $*.ppo

.ml.cmx:
	camlp5r $< -o $*.ppo
	$(OCAMLOPT) $(OCARGS) -c -impl $*.ppo
	rm -f $*.ppo

.mli.cmi:
	camlp5r $< -o $*.ppi
	$(OCAMLC) $(OCARGS) -c -intf $*.ppi
	rm -f $*.ppi

main.cmo: dag2html.cmo
dag2html.cmo: dag2html.cmi
dag2html.cmx: dag2html.cmi
