ROOT=..
include $(ROOT)/tools/Makefile.config

# First rule 'all' is defined in Makefile.config.

OCAMLI=-I ../wserver -I ../src
OBJS=../wserver/httpStatus.cmo ../wserver/wserver.cmo ../src/version.cmo ../src/argl.cmo ../src/buff.cmo ../src/translate.cmo setup.cmo

out: setup.out
	$(RM) gwsetup
	cp setup.out gwsetup

opt: setup.opt
	$(RM) gwsetup
	cp setup.opt gwsetup
	$(STRIP) gwsetup

setup.out: $(OBJS)
	$(OCAMLC) -custom $(STATIC) unix.cma $(LIBUNIX) $(OBJS) -o setup.out

setup.opt: $(OBJS:.cmo=.cmx)
	$(OCAMLOPT) $(STATIC) unix.cmxa $(LIBUNIX) $(OBJS:.cmo=.cmx) -o setup.opt

.PHONY: clean
clean:
	$(RM) *.out *.opt *.cm[oixa] *.cmxa *.pp[oi] *.o *.a *.obj *.lib \
	*.lck *.bak *~ .#* *.annot
	$(RM) gwsetup *.bat
	$(RM) -r test

.PHONY: depend
depend:
	$(ROOT)/tools/camlp5_depend.sh $(CAMLP5F) $(OCAMLI) -- *.ml* > .depend

.depend:
	@if [ ! -e $@ ]; then $(MAKE) -o $@ depend; fi

ifeq ($(filter $(MAKECMDGOALS),clean depend),)
  include .depend
endif
