# $Id: Makefile,v 3.13 2001-02-05 12:18:46 ddr Exp $

VERSION=3.11
RELEASE=1
DESTDIR=/usr/src/redhat

all:
	@echo "make install (as root)"

install:
	sed -e "s/VERSION/$(VERSION)/" -e "s/RELEASE/$(RELEASE)/" geneweb.spec > $(DESTDIR)/SPECS/geneweb-$(VERSION).spec
