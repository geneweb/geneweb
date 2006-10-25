# $Id: Makefile.inc,v 5.3 2006-10-25 17:25:39 ddr Exp $

TOP=..
CAMLP4_COMM=TOP=$(TOP) $(TOP)/tools/camlp4_comm.sh $(PPDEF)
CAMLP4F=-DUNIX
PPDEF=$(TOP)/src/ppdef.cmo
CAMLP4D=$$(camlp4 -where)
LIBUNIX=-cclib -lunix
OCAMLC=ocamlc.opt -warn-error A
OCAMLOPT=ocamlopt.opt -warn-error A
OCAMLI=
OCAMLD=
STRIP=strip
RM=/bin/rm -f
EXE=
STATIC=-ccopt -static
STATIC=
TEST_DIR=test $$(basename "$<") = "$<" || { echo "Please run 'make' in directory '$$(dirname "$<")' first"; exit 1; }

all::
opt::

clean::
	$(RM) *.out *.opt *.cm[oixa] *.cmxa *.pp[oi] *.o *.a *.obj *.lib *.lck *.bak *~ .#*

.SUFFIXES: .ml .cmo .mli .cmi .cmx

.mli.cmi:
	@$(TEST_DIR)
	@$(CAMLP4_COMM) $(CAMLP4F) $< -o $*.ppi
	$(OCAMLC) $(OCAMLI) -I +camlp4 -c -intf $*.ppi
	$(RM) $*.ppi

.ml.cmo:
	@$(TEST_DIR)
	@$(CAMLP4_COMM) $(CAMLP4F) $< -o $*.ppo
	$(OCAMLC) $(OCAMLI) $(OCAMLD) -I +camlp4 -c -impl $*.ppo
	$(RM) $*.ppo

.ml.cmx:
	@$(TEST_DIR)
	@$(CAMLP4_COMM) $(CAMLP4F) $< -o $*.ppo
	$(OCAMLOPT) $(OCAMLI) -I +camlp4 -c -impl $*.ppo
	$(RM) $*.ppo
