# $Id: Makefile,v 3.11 2000-11-06 13:45:00 ddr Exp $

VERSION=3.09
RELEASE=1
DESTDIR=/usr/src/redhat

all:
	@echo "make install (as root)"

install:
	sed -e "s/VERSION/$(VERSION)/" -e "s/RELEASE/$(RELEASE)/" geneweb.spec > $(DESTDIR)/SPECS/geneweb-$(VERSION).spec
	cp geneweb-initrc.sh $(DESTDIR)/SOURCES/geneweb-initrc-$(VERSION).sh
