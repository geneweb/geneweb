# $Id: Makefile,v 4.0 2001-03-16 19:33:03 ddr Exp $

VERSION=4.00
RELEASE=1
DESTDIR=/usr/src/RPM

all:
	@echo "make install (as root)"

install:
	sed -e "s/VERSION/$(VERSION)/" -e "s/RELEASE/$(RELEASE)/" geneweb.spec > $(DESTDIR)/SPECS/geneweb-$(VERSION).spec
