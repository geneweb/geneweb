# $Id: Makefile,v 4.8 2002-07-21 06:41:05 ddr Exp $

VERSION=4.07
RELEASE=1
DESTDIR=/usr/src/RPM

all:
	@echo "make install (as root)"

install:
	sed -e "s/VERSION/$(VERSION)/" -e "s/RELEASE/$(RELEASE)/" geneweb.spec > $(DESTDIR)/SPECS/geneweb-$(VERSION)-$(RELEASE).spec
