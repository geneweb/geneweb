# $Id: Makefile,v 3.12 2000-11-07 05:25:24 ddr Exp $

VERSION=3.10
RELEASE=1
DESTDIR=/usr/src/redhat

all:
	@echo "make install (as root)"

install:
	sed -e "s/VERSION/$(VERSION)/" -e "s/RELEASE/$(RELEASE)/" geneweb.spec > $(DESTDIR)/SPECS/geneweb-$(VERSION).spec
