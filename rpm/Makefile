# $Id: Makefile,v 1.4 1999-10-02 16:17:02 ddr Exp $

VERSION=2.07
RELEASE=1
REDHAT=/usr/src/redhat

all:
	@echo "make install (as root)"

install:
	sed -e "s/VERSION/$(VERSION)/" -e "s/RELEASE/$(RELEASE)/" geneweb.spec > $(REDHAT)/SPECS/geneweb-$(VERSION).spec
	cp geneweb-initrc.sh $(REDHAT)/SOURCES/geneweb-initrc-$(VERSION).sh
