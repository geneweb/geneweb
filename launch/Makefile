# $Id: Makefile,v 1.1 2006-10-12 10:25:49 ddr Exp $
# Copyright (c) 2006 INRIA

EXE=.out
TARGET=launch$(EXE)

all: $(TARGET)

$(TARGET): launch.cmo
	ocamlc -I +labltk labltk.cma unix.cma launch.cmo -o $(TARGET)

clean:
	rm -f *.cm[io] *.out *.log STOP_SERVER

%.cmo: %.ml
	ocamlc -pp camlp4r -I +labltk -c $<
