# $Id: Makefile,v 3.3 2000-03-22 10:51:17 ddr Exp $

VERSION=3.03
RELEASE=1
REDHAT=/usr/src/redhat

all:
	@echo "make install (as root)"

install:
	sed -e "s/VERSION/$(VERSION)/" -e "s/RELEASE/$(RELEASE)/" geneweb.spec > $(REDHAT)/SPECS/geneweb-$(VERSION).spec
	cp geneweb-initrc.sh $(REDHAT)/SOURCES/geneweb-initrc-$(VERSION).sh
