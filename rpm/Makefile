# $Id: Makefile,v 4.10 2002-12-30 18:40:03 ddr Exp $

VERSION=4.09
RELEASE=1
DESTDIR=/usr/src/RPM

all:
	@echo "make install (as root)"

install:
	sed -e "s/VERSION/$(VERSION)/" -e "s/RELEASE/$(RELEASE)/" geneweb.spec > $(DESTDIR)/SPECS/geneweb-$(VERSION)-$(RELEASE).spec
