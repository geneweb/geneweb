# $Id: Makefile,v 4.12 2004-12-14 10:22:05 ddr Exp $

VERSION=4.10
RELEASE=1
DESTDIR=/usr/src/RPM

all:
	@echo "make install (as root)"

install:
	sed -e "s/VERSION/$(VERSION)/" -e "s/RELEASE/$(RELEASE)/" geneweb.spec > $(DESTDIR)/SPECS/geneweb-$(VERSION)-$(RELEASE).spec
