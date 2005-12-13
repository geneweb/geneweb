# $Id: geneweb.spec,v 5.0 2005-12-13 11:51:26 ddr Exp $

Summary: Genealogy software with a Web interface
Name: geneweb
Version: VERSION
Release: RELEASE
Copyright: GPL
Vendor: INRIA
Group: Applications
Source: ftp://ftp.inria.fr/INRIA/Projects/cristal/geneweb/Src/geneweb-%{version}.tar.gz
URL: http://www.geneweb.org/
Packager: Daniel de Rauglaudre <daniel.de_rauglaudre@inria.fr>
# Requires: ld-linux.so.2 libc.so.6 libm.so.6 libm.so.6(GLIBC_2.1) libm.so.6(GLIBC_2.0) libc.so.6(GLIBC_2.1) libc.so.6(GLIBC_2.0)
BuildRoot: /tmp/%{name}-%{version}

Prefix: /usr
Summary(de): eine genealogische Software mit einem Web-Interface
Summary(fr): un logiciel de généalogie doté d'une interface Web
Summary(nl): een genealogisch programma met een www-interface
Summary(se): ett genealogi program med ett webbinterface

%description
GeneWeb is a genealogy software with a Web interface. It can be used
off-line or as a Web service.

%description -l de
GeneWeb ist eine genealogische Software mit einem Web-Interface.
Sie kann off-line oder als ein Web-Service genutzt werden.

%description -l fr
GeneWeb est un logiciel de généalogie doté d'une interface Web. Il
peut être utilisé non connecté au réseau ou comme un service Web.

%description -l nl
GeneWeb is een genealogisch programma met een www-interface, dat kan
gebruikt worden op computers met of zonder permanente verbinding met
het Internet.

%description -l se
GeneWeb är ett genealogi program med ett webbinterface. Det kan användas 
nedkopplad eller som en webbtjänst.

%prep
rm -rf $RPM_BUILD_ROOT
%setup

%build
make opt
make distrib

%install
mkdir -p $RPM_BUILD_ROOT/home/geneweb
mkdir -p $RPM_BUILD_ROOT/etc/rc.d/init.d
mkdir -p $RPM_BUILD_ROOT/etc/logrotate.d
cp -r distribution $RPM_BUILD_ROOT/home/geneweb/gw
cp rpm/geneweb-initrc.sh $RPM_BUILD_ROOT/etc/rc.d/init.d/gwd
cp rpm/geneweb-logrotate $RPM_BUILD_ROOT/etc/logrotate.d/gwd

%clean
make clean
rm -rf $RPM_BUILD_ROOT

%pre
/usr/sbin/groupadd geneweb || :
/usr/sbin/useradd -d /home/geneweb -g geneweb -c "GeneWeb database" geneweb || :
chmod a+rx /home/geneweb

%post
chown -R geneweb.geneweb /home/geneweb/gw
# SuSE compatibility
if (test -e "/etc/SuSE-release") ; then
  ln -s /etc/rc.d/init.d/gwd /etc/init.d/gwd
fi
#end
chkconfig --add gwd
/etc/rc.d/init.d/gwd start

%preun
/etc/rc.d/init.d/gwd stop
chkconfig --del gwd
rm -f /etc/init.d/gwd

%postun
# cd /home/geneweb/gw/gw
# rm -rf doc etc images lang setup gwtp_tmp

%files
%defattr(-,geneweb,geneweb)
%attr(6750, geneweb, geneweb) /home/geneweb/gw/gw/gwd
%attr(6750, geneweb, geneweb) /home/geneweb/gw/gw/gwsetup
%attr(6750, geneweb, geneweb) /home/geneweb/gw/gw/ged2gwb
%attr(6750, geneweb, geneweb) /home/geneweb/gw/gw/gwb2ged
%attr(6750, geneweb, geneweb) /home/geneweb/gw/gw/gwc
%attr(6750, geneweb, geneweb) /home/geneweb/gw/gw/gwu
%attr(6750, geneweb, geneweb) /home/geneweb/gw/gw/consang
%attr(6750, geneweb, geneweb) /home/geneweb/gw/gw/gwtp_tmp/gwtp
%attr(755, root, root) /etc/rc.d/init.d/gwd
%attr(644, root, root) /etc/logrotate.d/gwd
/home/geneweb/gw/LICENSE.txt
/home/geneweb/gw/LISEZMOI.txt
/home/geneweb/gw/README.txt
/home/geneweb/gw/gw/CHANGES.txt
/home/geneweb/gw/gw/INSTALL.htm
/home/geneweb/gw/gw/LICENSE.txt
/home/geneweb/gw/gw/LISEZMOI.txt
/home/geneweb/gw/gw/README.txt
/home/geneweb/gw/gw/a.gwf
/home/geneweb/gw/gw/doc
/home/geneweb/gw/gw/etc
/home/geneweb/gw/gw/gwtp_tmp/README
/home/geneweb/gw/gw/gwtp_tmp/lang
/home/geneweb/gw/gw/images
/home/geneweb/gw/gw/lang
/home/geneweb/gw/gw/only.txt
/home/geneweb/gw/gw/setup
/home/geneweb/gw/gwd
/home/geneweb/gw/gwsetup
%defattr(-,root,root)
#%doc doc/*

%changelog
* Sat Nov 10 2000 Daniel de Rauglaudre
- Used chkconfig (Eddie Bindt's hint)

* Thu Nov  9 2000 Ludovic Ledieu
- Simplified files list (a directory includes its contents). It's thus easier
  to maintain the list.
- Removed doc in /usr/doc/geneweb-<version>

* Tue Nov  7 2000 Daniel de Rauglaudre
Version 3.10
- No more installation of geneweb-initrc.sh in SOURCES: it is directly
  copied from BUILD/geneweb-xx/rpm. In that file, added -log for gwd and
  gwsetup.
- Added /etc/logrotate.d/gwd

* Sun Apr  9 2000 Daniel de Rauglaudre
Version 3.03-2
- added set user id bit also for gwc gwu ged2gwb gwb2ged consang because
  were not launched as geneweb user (seemed to depend on the Linux version)

* Tue Dec  7 1999 Daniel de Rauglaudre
Version 3.01
- updated the file list

* Mon Oct  4 1999 Daniel de Rauglaudre
Version 2.07-2
- added RPM_BUILD_ROOT to %install the package in /tmp

* Sun Sep 6 1999 Daniel de Rauglaudre
Version 2.06-5
- updated file list

* Sun Sep 5 1999 Daniel de Rauglaudre
Version 2.06-4
Deleted option -r in useradd (Redhat specific)

* Sat Aug 14 1999 Dan Kegel <dank@alumni.caltech.edu>
Created.
