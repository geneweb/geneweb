# $Id: Makefile,v 3.7 2000-07-12 12:56:15 ddr Exp $

VERSION=3.06
RELEASE=1
REDHAT=/usr/src/redhat

all:
	@echo "make install (as root)"

install:
	sed -e "s/VERSION/$(VERSION)/" -e "s/RELEASE/$(RELEASE)/" geneweb.spec > $(REDHAT)/SPECS/geneweb-$(VERSION).spec
	cp geneweb-initrc.sh $(REDHAT)/SOURCES/geneweb-initrc-$(VERSION).sh
