ROOT=..
include $(ROOT)/tools/Makefile.config

BINDIR=$(PREFIX)/bin
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

.PHONY: depend
depend:
	$(ROOT)/tools/camlp5_depend.sh $(CAMLP5F) $(OCAMLI) -- *.ml* > .depend

.depend:
	@if [ ! -e $@ ]; then $(MAKE) -o $@ depend; fi

ifeq ($(filter $(MAKECMDGOALS),clean depend),)
  include .depend
endif
