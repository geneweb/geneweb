# $Id: Makefile,v 3.10 2000-10-02 13:21:36 ddr Exp $

VERSION=3.08
RELEASE=1
DESTDIR=/usr/src/redhat

all:
	@echo "make install (as root)"

install:
	sed -e "s/VERSION/$(VERSION)/" -e "s/RELEASE/$(RELEASE)/" geneweb.spec > $(DESTDIR)/SPECS/geneweb-$(VERSION).spec
	cp geneweb-initrc.sh $(DESTDIR)/SOURCES/geneweb-initrc-$(VERSION).sh
