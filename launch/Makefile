# $Id: Makefile,v 1.3 2006-11-20 11:41:55 ddr Exp $
# Copyright (c) 2006 INRIA

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
	export LC_ALL=C; TOP=$(GWB) $(GWB)/tools/camlp4_depend.sh $(OCAMLI) $$(ls *.mli *.ml) | sed -e 's|\.\.|$$(GWB)|g' > .depend.new
	mv .depend.new .depend

%.cmo: %.ml
	@$(TEST_DIR)
	ocamlc -pp camlp4r $(OCAMLI) -I +labltk -c $<

include .depend
