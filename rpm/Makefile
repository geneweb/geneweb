# $Id: Makefile,v 4.4 2001-08-28 13:41:42 ddr Exp $

VERSION=4.03
RELEASE=1
DESTDIR=/usr/src/RPM

all:
	@echo "make install (as root)"

install:
	sed -e "s/VERSION/$(VERSION)/" -e "s/RELEASE/$(RELEASE)/" geneweb.spec > $(DESTDIR)/SPECS/geneweb-$(VERSION)-$(RELEASE).spec
