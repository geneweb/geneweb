# $Id: Makefile,v 1.2 2006-10-17 13:35:05 ddr Exp $
# Copyright (c) 2006 INRIA

EXE=.out
TARGET=launch$(EXE)

all: $(TARGET)

$(TARGET): launch.cmo
	ocamlc -I +labltk labltk.cma unix.cma launch.cmo -o $(TARGET)

clean:
	rm -f *.cm[io] *.out *.log

%.cmo: %.ml
	ocamlc -pp camlp4r -I +labltk -c $<
