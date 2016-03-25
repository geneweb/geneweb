# $Id: Makefile,v 1.6 2007-09-12 09:58:44 ddr Exp $
# Copyright (c) 2006-2007 INRIA

EXE=.out
GWB=..
OBJS=../src/iovalue.cmo ../src/buff.cmo ../src/name.cmo ../src/mutil.cmo launch.cmo
OCAMLI=-I ../src
TARGET=launch$(EXE)
TEST_DIR=test $$(basename "$<") = "$<" || { echo "Please run 'make' in directory '$$(dirname "$<")' first"; exit 1; }

all: $(TARGET)

$(TARGET): $(OBJS)
	ocamlc -I +labltk labltk.cma unix.cma $(OBJS) -o $(TARGET)

%.cmo: %.ml
	@$(TEST_DIR)
	ocamlc -pp camlp5r $(OCAMLI) -I +labltk -c $<

clean:
	rm -f *.cm[io] *.out *.log

.PHONY: depend
depend:
	export LC_ALL=C; TOP=$(GWB) $(GWB)/tools/camlp5_depend.sh $(OCAMLI) $$(ls *.mli *.ml) | sed -e 's|\.\.|$$(GWB)|g' > .depend

.depend:
	@if [ ! -e $@ ]; then $(MAKE) -o $@ depend; fi

ifeq ($(filter $(MAKECMDGOALS),clean depend),)
  include .depend
endif
