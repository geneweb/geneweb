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

clean:
	rm -f *.cm[io] *.out *.log

depend:
	export LC_ALL=C; TOP=$(GWB) $(GWB)/tools/camlp5_depend.sh $(OCAMLI) $$(ls *.mli *.ml) | sed -e 's|\.\.|$$(GWB)|g' > .depend.new
	mv .depend.new .depend

%.cmo: %.ml
	@$(TEST_DIR)
	ocamlc -pp camlp5r $(OCAMLI) -I +labltk -c $<

include .depend
