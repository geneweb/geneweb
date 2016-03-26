ROOT=..
include $(ROOT)/tools/Makefile.config

# First rule 'all' is defined in Makefile.config.

OCAMLI=-I ../src

GWTP_OTHER=../src/lock.cmo ../src/buff.cmo ../src/name.cmo \
../src/iovalue.cmo ../src/adef.cmo ../src/mutil.cmo ../src/futil.cmo \
../src/dutil.cmo ../src/secure.cmo ../src/btree.cmo \
../src/database.cmo ../src/db2.cmo ../src/db2disk.cmo ../src/diff.cmo \
../src/outbase.cmo ../src/gwdb.cmo ../src/checkItem.cmo \
../src/gutil.cmo ../src/translate.cmo \

GWTP_OBJS=httpEnv.cmo iolight.cmo gwtp.cmo

OBJS=$(GWTP_OTHER) $(GWTP_OBJS)

RECOVER_OBJS=$(GWTP_OTHER) iolight.cmo recover.cmo

out: gwtp.out
	$(RM) gwtp
	cp gwtp.out gwtp

opt: gwtp.opt
	$(RM) gwtp
	cp gwtp.opt gwtp
	$(STRIP) gwtp

recover: recover.out

gwtp.out: $(GWTP_OBJS)
	$(OCAMLC) -custom $(STATIC) unix.cma $(OBJS) $(LIBUNIX) -o gwtp.out

gwtp.opt: $(GWTP_OBJS:.cmo=.cmx)
	$(OCAMLOPT) $(STATIC) unix.cmxa $(OBJS:.cmo=.cmx) $(LIBUNIX) -o gwtp.opt

recover.out: $(RECOVER_OBJS)
	$(OCAMLC) unix.cma $(RECOVER_OBJS) $(LIBUNIX) -o $@

.PHONY: clean
clean:
	$(RM) *.out *.opt *.cm[oixa] *.cmxa *.pp[oi] *.o *.a *.obj *.lib \
	*.lck *.bak *~ .#* *.annot
	$(RM) gwtp

.PHONY: depend
depend:
	$(ROOT)/tools/camlp5_depend.sh $(CAMLP5F) $(OCAMLI) -- *.ml* > .depend

.depend:
	@if [ ! -e $@ ]; then $(MAKE) -o $@ depend; fi

ifeq ($(filter $(MAKECMDGOALS),clean depend),)
  include .depend
endif
