# $Id: Makefile,v 4.7 2002-02-28 08:54:17 ddr Exp $

VERSION=4.06
RELEASE=1
DESTDIR=/usr/src/RPM

all:
	@echo "make install (as root)"

install:
	sed -e "s/VERSION/$(VERSION)/" -e "s/RELEASE/$(RELEASE)/" geneweb.spec > $(DESTDIR)/SPECS/geneweb-$(VERSION)-$(RELEASE).spec
