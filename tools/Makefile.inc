# $Id: Makefile.inc,v 4.0 2001-03-16 19:35:13 ddr Exp $

CAMLP4_COMM=../tools/camlp4_comm.sh pa_ifdef.cmo pa_ru.cmo
CAMLP4F=-DUNIX
CAMLP4D=`camlp4 -where`
LIBUNIX=-cclib -lunix
OCAMLC=ocamlc.opt
OCAMLOPT=ocamlopt.opt
OCAMLI=
OCAMLD=
STRIP=strip
RM=/bin/rm -f
EXE=

all::
opt::

clean::
	$(RM) *.out *.opt *.cm[oixa] *.cmxa *.pp[oi] *.o *.a *.obj *.lib *.lck *.bak *~ .#*

.SUFFIXES: .ml .cmo .mli .cmi .cmx

.mli.cmi:
	@if test `basename $<` != $<; then echo "Bad directory for $<"; exit 1; fi
	@$(CAMLP4_COMM) $(CAMLP4F) $< -o $*.ppi
	$(OCAMLC) $(OCAMLI) -I $(CAMLP4D) -c -intf $*.ppi
	$(RM) $*.ppi

.ml.cmo:
	@if test `basename $<` != $<; then echo "Bad directory for $<"; exit 1; fi
	@$(CAMLP4_COMM) $(CAMLP4F) $< -o $*.ppo
	$(OCAMLC) $(OCAMLI) $(OCAMLD) -I $(CAMLP4D) -c -impl $*.ppo
	$(RM) $*.ppo

.ml.cmx:
	@if test `basename $<` != $<; then echo "Bad directory for $<"; exit 1; fi
	@$(CAMLP4_COMM) $(CAMLP4F) $< -o $*.ppo
	$(OCAMLOPT) $(OCAMLI) -I $(CAMLP4D) -c -impl $*.ppo
	$(RM) $*.ppo
