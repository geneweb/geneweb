# $Id: geneweb.spec,v 3.20 2000-12-19 15:42:09 ddr Exp $
#
# geneweb .spec file -- 15 August 1999 -- Dan Kegel
#
# This .spec file is commented to help maintainers who are not
# yet intimately familliar with the process of creating RPM's.
# Like me :-)
#
# First rule: buy a copy of Maximum RPM and read it.
# 
# .spec header lines that describe the rpm are gathered at the top:
#
# Note: the resulting .rpm is named $name-$version.$release.rpm
# e.g. if 'Version' is 2.06, and 'Release' is 1, it'll be geneweb-2.06-1.rpm
# The resulting source .srpm is named $name-$version.$release.srpm
# 'Release' refers only to the .rpm, not to the source .tar.gz;
# it starts at 1 for the first .rpm released for a given source version,
# and should be incremented each time a new .rpm is released.

Summary: Genealogy software with a Web interface
Name: geneweb
Version: VERSION
Release: RELEASE
Copyright: GPL
Vendor: INRIA
Group: Applications
Source: ftp://ftp.inria.fr/INRIA/Projects/cristal/geneweb/Src/geneweb-%{version}.tar.gz
URL: http://cristal.inria.fr/~ddr/GeneWeb/
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

# *********** BUILDING .RPM *************
# Now come the header lines that describe how to build the application
# from source and turn it into an .rpm
# rpm -b runs the %prep and %build scripts.
# This stuff only happens on the developer's machine.

# %prep: before the build.  
# Blow away temporaries from last aborted build if any.
# Unpack the .tar.gz (using %setup).
%prep
rm -rf $RPM_BUILD_ROOT
%setup

# %build: how to compile
%build
make opt
make distrib

# %install: after compiling.  put the geneweb distrib folder
# into the geneweb user's gw subdir, then set up the /etc/rc.d entries.
# (Note: this isn't the same kind of install that the end-user does.
#  This sets up the same files 'by hand'; rpm will then archive them.
#  The end user installs the copies from the .rpm archive.)
%install
mkdir -p $RPM_BUILD_ROOT/home/geneweb
mkdir -p $RPM_BUILD_ROOT/etc/rc.d/init.d
mkdir -p $RPM_BUILD_ROOT/etc/logrotate.d
cp -r distribution $RPM_BUILD_ROOT/home/geneweb/gw
cp rpm/geneweb-initrc.sh $RPM_BUILD_ROOT/etc/rc.d/init.d/gwd
cp rpm/geneweb-logrotate $RPM_BUILD_ROOT/etc/logrotate.d/gwd

# %clean: after installing, how to clean up.  (The files are all
# in the .rpm archive by now.  Need to remove them before we
# can test the whole thing with 'rpm -i foo.rpm'.)
%clean
make clean
rm -rf $RPM_BUILD_ROOT

# *********** INSTALLING .RPM *************
# This stuff only happens on the user's machine.
# rpm -i runs the %pre script, in which I create the geneweb user,
# then it automatically unpacks all the files and symlinks from the archive.
# Finally it runs the %post script, in which I start the service.
%pre
/usr/sbin/groupadd geneweb || :
/usr/sbin/useradd -d /home/geneweb -g geneweb -c "GeneWeb database" geneweb || :
chmod a+rx /home/geneweb

%post
# Sure, all the files are already owned by geneweb, but the directories ain't.
chown -R geneweb.geneweb /home/geneweb/gw
chkconfig --add gwd
/etc/rc.d/init.d/gwd start

# *********** UNINSTALLING .RPM *************
# rpm -e automatically erases all the files listed in %files.
# Beforehand, it runs the %preun script; afterwards, it runs the %postun
# script.  I use them to stop the service & remove the pseudouser.
%preun
/etc/rc.d/init.d/gwd stop
chkconfig --del gwd
(
  cd /home/geneweb/gw/gw
  set *.gwb
  if test -d "$1"; then
    mkdir -p /home/geneweb/gw-%{version}
    cp gwu gwb2ged /home/geneweb/gw-%{version}/.
    for i in $*; do
      rm -rf /home/geneweb/gw-%{version}/$i
      mv $i /home/geneweb/gw-%{version}/.
    done
    rm -f *.lck
    echo
    echo "Warning: the following data bases:"
    for i in $*; do
      echo -n "   "
      echo $i
    done
    echo "have been moved to the directory:"
    echo -n "   "
    echo "/home/geneweb/gw-%{version}"
    echo
    echo "Remember this directory name for further possible recovery."
    echo
  fi
)

%postun
(rmdir /home/geneweb/gw/gw/doc/* >/dev/null 2>&1; exit 0)
(rmdir /home/geneweb/gw/gw/doc >/dev/null 2>&1; exit 0)
(rmdir /home/geneweb/gw/gw/etc >/dev/null 2>&1; exit 0)
(rmdir /home/geneweb/gw/gw/images >/dev/null 2>&1; exit 0)
(rmdir /home/geneweb/gw/gw/lang/* >/dev/null 2>&1; exit 0)
(rmdir /home/geneweb/gw/gw/lang > /dev/null 2>&1; exit 0)
(rmdir /home/geneweb/gw/gw/setup/* >/dev/null 2>&1; exit 0)
(rmdir /home/geneweb/gw/gw/setup >/dev/null 2>&1; exit 0)

# *********** THE FILES OWNED BY THIS .RPM *************
# These are the files belonging to this package.  We have to list
# them so RPM can install and uninstall them.
# (If a line starts with %doc, it means that file goes into 
# /usr/doc/$packagename instead of ~geneweb.)
# This package is not relocatable, which kinda sucks.
# Note that gwd and gwsetup (the main daemon and the gwsetup daemon) are
# installed setuid, owned by geneweb, and can only be run by root.
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
/home/geneweb/gw/LISEZMOI.htm
/home/geneweb/gw/README.htm
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
