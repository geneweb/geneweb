# $Id: Makefile,v 1.1 1999-09-01 19:25:26 ddr Exp $

VERSION=2.06
RELEASE=2
REDHAT=/usr/src/redhat

all:
	@echo "make install VERSION=... RELEASE=... (as root)"

install:
	cat geneweb.spec | sed \
	-e "s/VERSION/$(VERSION)/g" \
	-e "s/RELEASE/$(RELEASE)/g" \
	> $(REDHAT)/SPECS/geneweb-$(VERSION).spec
	cp geneweb-initrc.sh $(REDHAT)/SOURCES/geneweb-initrc-$(VERSION).sh
