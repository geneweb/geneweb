ROOT=..
include $(ROOT)/tools/Makefile.config

# First rule 'all' is defined in Makefile.config.

.PHONY: out opt clean depend

OBJS=wserver.cmo

out: $(OBJS:.cmo=.cmx)

opt: $(OBJS:.cmo=.cmx)

clean:
	$(RM) *.out *.opt *.cm[oixa] *.cmxa *.pp[oi] *.o *.a *.obj *.lib \
	*.lck *.bak *~ .#* *.annot

depend:
	$(ROOT)/tools/camlp5_depend.sh $(OCAMLI) *.ml* > .depend

include .depend
