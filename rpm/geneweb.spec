# $Id: geneweb.spec,v 1.17 1999-09-25 21:23:02 ddr Exp $
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
Source1: geneweb-initrc-%{version}.sh
URL: http://cristal.inria.fr/~ddr/GeneWeb/
Packager: Daniel de Rauglaudre <daniel.de_rauglaudre@inria.fr>
# Requires: ld-linux.so.2 libc.so.6 libm.so.6 libncurses.so.4 libm.so.6(GLIBC_2.1) libm.so.6(GLIBC_2.0) libc.so.6(GLIBC_2.1) libc.so.6(GLIBC_2.0)

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
# Delete any stray CVS dirs that got included (they break 'make install')
%prep
rm -f /etc/rc.d/rc?.d/S99gwd /etc/rc.d/rc?.d/K01gwd
rm -rf /home/geneweb/gw /usr/doc/geneweb-%{version}
%setup
find . -name CVS -print | /usr/bin/xargs /bin/rm -rf 

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
mkdir -p /home/geneweb
cp -r distribution /home/geneweb/gw
cp $RPM_SOURCE_DIR/geneweb-initrc-%{version}.sh /etc/rc.d/init.d/gwd
ln -s ../init.d/gwd /etc/rc.d/rc0.d/K01gwd
ln -s ../init.d/gwd /etc/rc.d/rc1.d/K01gwd
ln -s ../init.d/gwd /etc/rc.d/rc2.d/S99gwd
ln -s ../init.d/gwd /etc/rc.d/rc3.d/S99gwd
ln -s ../init.d/gwd /etc/rc.d/rc5.d/S99gwd
ln -s ../init.d/gwd /etc/rc.d/rc6.d/K01gwd

# %clean: after installing, how to clean up.  (The files are all
# in the .rpm archive by now.  Need to remove them before we
# can test the whole thing with 'rpm -i foo.rpm'.)
%clean
make clean
rm -rf /home/geneweb/gw /usr/doc/geneweb-%{version} /etc/rc.d/*/*gwd

# *********** INSTALLING .RPM *************
# This stuff only happens on the user's machine.
# rpm -i runs the %pre script, in which I create the geneweb user,
# then it automatically unpacks all the files and symlinks from the archive.
# Finally it runs the %post script, in which I start the service.
%pre
/usr/sbin/useradd -d /home/geneweb -c "GeneWeb database" geneweb
chmod a+rx /home/geneweb

%post
# Sure, all the files are already owned by geneweb, but the directories ain't.
chown -R geneweb.geneweb /home/geneweb/gw
/etc/rc.d/init.d/gwd start

# *********** UNINSTALLING .RPM *************
# rpm -e automatically erases all the files listed in %files.
# Beforehand, it runs the %preun script; afterwards, it runs the %postun
# script.  I use them to stop the service & remove the pseudouser.
%preun
/etc/rc.d/init.d/gwd stop
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
/usr/sbin/userdel geneweb

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
%attr(744, root, root) /etc/rc.d/init.d/gwd
%attr(744, root, root) /etc/rc.d/rc0.d/K01gwd
%attr(744, root, root) /etc/rc.d/rc1.d/K01gwd
%attr(744, root, root) /etc/rc.d/rc2.d/S99gwd
%attr(744, root, root) /etc/rc.d/rc3.d/S99gwd
%attr(744, root, root) /etc/rc.d/rc5.d/S99gwd
%attr(744, root, root) /etc/rc.d/rc6.d/K01gwd
/home/geneweb/gw/LISEZMOI.txt
/home/geneweb/gw/README.txt
/home/geneweb/gw/gw/CHANGES
/home/geneweb/gw/gw/INSTALL.htm
/home/geneweb/gw/gw/LICENSE
/home/geneweb/gw/gw/LISEZMOI.txt
/home/geneweb/gw/gw/README.txt
/home/geneweb/gw/gw/a.gwf
/home/geneweb/gw/gw/consang
/home/geneweb/gw/gw/doc/CREDITS.htm
/home/geneweb/gw/gw/doc/LICENSE.htm
/home/geneweb/gw/gw/doc/de/consang.htm
/home/geneweb/gw/gw/doc/de/diruse.htm
/home/geneweb/gw/gw/doc/de/faq.htm
/home/geneweb/gw/gw/doc/de/links.htm
/home/geneweb/gw/gw/doc/de/maint.htm
/home/geneweb/gw/gw/doc/de/merge.htm
/home/geneweb/gw/gw/doc/de/pcustom.htm
/home/geneweb/gw/gw/doc/de/problem.htm
/home/geneweb/gw/gw/doc/de/recover.htm
/home/geneweb/gw/gw/doc/de/report.htm
/home/geneweb/gw/gw/doc/de/server.htm
/home/geneweb/gw/gw/doc/de/start.htm
/home/geneweb/gw/gw/doc/de/update.htm
/home/geneweb/gw/gw/doc/en/access.htm
/home/geneweb/gw/gw/doc/en/consang.htm
/home/geneweb/gw/gw/doc/en/diruse.htm
/home/geneweb/gw/gw/doc/en/faq.htm
/home/geneweb/gw/gw/doc/en/links.htm
/home/geneweb/gw/gw/doc/en/maint.htm
/home/geneweb/gw/gw/doc/en/merge.htm
/home/geneweb/gw/gw/doc/en/pcustom.htm
/home/geneweb/gw/gw/doc/en/problem.htm
/home/geneweb/gw/gw/doc/en/recover.htm
/home/geneweb/gw/gw/doc/en/report.htm
/home/geneweb/gw/gw/doc/en/server.htm
/home/geneweb/gw/gw/doc/en/start.htm
/home/geneweb/gw/gw/doc/en/update.htm
/home/geneweb/gw/gw/doc/fr/access.htm
/home/geneweb/gw/gw/doc/fr/consang.htm
/home/geneweb/gw/gw/doc/fr/diruse.htm
/home/geneweb/gw/gw/doc/fr/faq.htm
/home/geneweb/gw/gw/doc/fr/links.htm
/home/geneweb/gw/gw/doc/fr/maint.htm
/home/geneweb/gw/gw/doc/fr/merge.htm
/home/geneweb/gw/gw/doc/fr/pcustom.htm
/home/geneweb/gw/gw/doc/fr/problem.htm
/home/geneweb/gw/gw/doc/fr/recover.htm
/home/geneweb/gw/gw/doc/fr/report.htm
/home/geneweb/gw/gw/doc/fr/server.htm
/home/geneweb/gw/gw/doc/fr/start.htm
/home/geneweb/gw/gw/doc/fr/update.htm
/home/geneweb/gw/gw/doc/index.htm
/home/geneweb/gw/gw/doc/nl/consang.htm
/home/geneweb/gw/gw/doc/nl/diruse.htm
/home/geneweb/gw/gw/doc/nl/faq.htm
/home/geneweb/gw/gw/doc/nl/links.htm
/home/geneweb/gw/gw/doc/nl/maint.htm
/home/geneweb/gw/gw/doc/nl/merge.htm
/home/geneweb/gw/gw/doc/nl/pcustom.htm
/home/geneweb/gw/gw/doc/nl/problem.htm
/home/geneweb/gw/gw/doc/nl/recover.htm
/home/geneweb/gw/gw/doc/nl/report.htm
/home/geneweb/gw/gw/doc/nl/server.htm
/home/geneweb/gw/gw/doc/nl/start.htm
/home/geneweb/gw/gw/doc/nl/update.htm
/home/geneweb/gw/gw/doc/se/access.htm
/home/geneweb/gw/gw/doc/se/consang.htm
/home/geneweb/gw/gw/doc/se/diruse.htm
/home/geneweb/gw/gw/doc/se/faq.htm
/home/geneweb/gw/gw/doc/se/links.htm
/home/geneweb/gw/gw/doc/se/maint.htm
/home/geneweb/gw/gw/doc/se/merge.htm
/home/geneweb/gw/gw/doc/se/pcustom.htm
/home/geneweb/gw/gw/doc/se/problem.htm
/home/geneweb/gw/gw/doc/se/recover.htm
/home/geneweb/gw/gw/doc/se/report.htm
/home/geneweb/gw/gw/doc/se/server.htm
/home/geneweb/gw/gw/doc/se/start.htm
/home/geneweb/gw/gw/doc/se/update.htm
/home/geneweb/gw/gw/etc/copyr.txt
/home/geneweb/gw/gw/etc/redirect.txt
/home/geneweb/gw/gw/etc/renamed.txt
/home/geneweb/gw/gw/etc/robot.txt
/home/geneweb/gw/gw/ged2gwb
/home/geneweb/gw/gw/gwb2ged
/home/geneweb/gw/gw/gwc
/home/geneweb/gw/gw/gwu
/home/geneweb/gw/gw/images/l-cn.jpg
/home/geneweb/gw/gw/images/l-cs.jpg
/home/geneweb/gw/gw/images/l-de.jpg
/home/geneweb/gw/gw/images/l-dk.jpg
/home/geneweb/gw/gw/images/l-en.jpg
/home/geneweb/gw/gw/images/l-eo.jpg
/home/geneweb/gw/gw/images/l-es.jpg
/home/geneweb/gw/gw/images/l-fr.jpg
/home/geneweb/gw/gw/images/l-he.jpg
/home/geneweb/gw/gw/images/l-it.jpg
/home/geneweb/gw/gw/images/l-nl.jpg
/home/geneweb/gw/gw/images/l-no.jpg
/home/geneweb/gw/gw/images/l-pt.jpg
/home/geneweb/gw/gw/images/l-se.jpg
/home/geneweb/gw/gw/images/up.jpg
/home/geneweb/gw/gw/lang/advanced.txt
/home/geneweb/gw/gw/lang/cn/start.txt
/home/geneweb/gw/gw/lang/cs/start.txt
/home/geneweb/gw/gw/lang/de/start.txt
/home/geneweb/gw/gw/lang/dk/start.txt
/home/geneweb/gw/gw/lang/en/start.txt
/home/geneweb/gw/gw/lang/eo/start.txt
/home/geneweb/gw/gw/lang/es/start.txt
/home/geneweb/gw/gw/lang/fr/start.txt
/home/geneweb/gw/gw/lang/he/start.txt
/home/geneweb/gw/gw/lang/it/start.txt
/home/geneweb/gw/gw/lang/lexicon.txt
/home/geneweb/gw/gw/lang/nl/start.txt
/home/geneweb/gw/gw/lang/no/start.txt
/home/geneweb/gw/gw/lang/pt/start.txt
/home/geneweb/gw/gw/lang/se/start.txt
/home/geneweb/gw/gw/lang/version.txt
/home/geneweb/gw/gw/only.txt
/home/geneweb/gw/gw/setup/de/backg.htm
/home/geneweb/gw/gw/setup/de/bsi.htm
/home/geneweb/gw/gw/setup/de/bsi_err.htm
/home/geneweb/gw/gw/setup/de/bso.htm
/home/geneweb/gw/gw/setup/de/bso_err.htm
/home/geneweb/gw/gw/setup/de/bso_ok.htm
/home/geneweb/gw/gw/setup/de/clean_ok.htm
/home/geneweb/gw/gw/setup/de/cleanup.htm
/home/geneweb/gw/gw/setup/de/cleanup1.htm
/home/geneweb/gw/gw/setup/de/consang.htm
/home/geneweb/gw/gw/setup/de/consg_ok.htm
/home/geneweb/gw/gw/setup/de/del_ok.htm
/home/geneweb/gw/gw/setup/de/delete.htm
/home/geneweb/gw/gw/setup/de/delete_1.htm
/home/geneweb/gw/gw/setup/de/err_acc.htm
/home/geneweb/gw/gw/setup/de/err_cnfl.htm
/home/geneweb/gw/gw/setup/de/err_miss.htm
/home/geneweb/gw/gw/setup/de/err_name.htm
/home/geneweb/gw/gw/setup/de/err_ndir.htm
/home/geneweb/gw/gw/setup/de/err_ngw.htm
/home/geneweb/gw/gw/setup/de/err_outd.htm
/home/geneweb/gw/gw/setup/de/err_reco.htm
/home/geneweb/gw/gw/setup/de/err_smdr.htm
/home/geneweb/gw/gw/setup/de/err_unkn.htm
/home/geneweb/gw/gw/setup/de/ged2gwb.htm
/home/geneweb/gw/gw/setup/de/gw2gd_ok.htm
/home/geneweb/gw/gw/setup/de/gwb2ged.htm
/home/geneweb/gw/gw/setup/de/gwc.htm
/home/geneweb/gw/gw/setup/de/gwd.htm
/home/geneweb/gw/gw/setup/de/gwd_info.htm
/home/geneweb/gw/gw/setup/de/gwd_ok.htm
/home/geneweb/gw/gw/setup/de/gwf.htm
/home/geneweb/gw/gw/setup/de/gwf_1.htm
/home/geneweb/gw/gw/setup/de/gwf_ok.htm
/home/geneweb/gw/gw/setup/de/gwu.htm
/home/geneweb/gw/gw/setup/de/gwu_ok.htm
/home/geneweb/gw/gw/setup/de/intro.txt
/home/geneweb/gw/gw/setup/de/list.htm
/home/geneweb/gw/gw/setup/de/main.htm
/home/geneweb/gw/gw/setup/de/note.htm
/home/geneweb/gw/gw/setup/de/recover.htm
/home/geneweb/gw/gw/setup/de/recover1.htm
/home/geneweb/gw/gw/setup/de/recover2.htm
/home/geneweb/gw/gw/setup/de/ren_ok.htm
/home/geneweb/gw/gw/setup/de/rename.htm
/home/geneweb/gw/gw/setup/de/save.htm
/home/geneweb/gw/gw/setup/de/simple.htm
/home/geneweb/gw/gw/setup/de/traces.htm
/home/geneweb/gw/gw/setup/de/welcome.htm
/home/geneweb/gw/gw/setup/en/backg.htm
/home/geneweb/gw/gw/setup/en/bsi.htm
/home/geneweb/gw/gw/setup/en/bsi_err.htm
/home/geneweb/gw/gw/setup/en/bso.htm
/home/geneweb/gw/gw/setup/en/bso_err.htm
/home/geneweb/gw/gw/setup/en/bso_ok.htm
/home/geneweb/gw/gw/setup/en/clean_ok.htm
/home/geneweb/gw/gw/setup/en/cleanup.htm
/home/geneweb/gw/gw/setup/en/cleanup1.htm
/home/geneweb/gw/gw/setup/en/consang.htm
/home/geneweb/gw/gw/setup/en/consg_ok.htm
/home/geneweb/gw/gw/setup/en/del_ok.htm
/home/geneweb/gw/gw/setup/en/delete.htm
/home/geneweb/gw/gw/setup/en/delete_1.htm
/home/geneweb/gw/gw/setup/en/err_acc.htm
/home/geneweb/gw/gw/setup/en/err_cnfl.htm
/home/geneweb/gw/gw/setup/en/err_miss.htm
/home/geneweb/gw/gw/setup/en/err_name.htm
/home/geneweb/gw/gw/setup/en/err_ndir.htm
/home/geneweb/gw/gw/setup/en/err_ngw.htm
/home/geneweb/gw/gw/setup/en/err_outd.htm
/home/geneweb/gw/gw/setup/en/err_reco.htm
/home/geneweb/gw/gw/setup/en/err_smdr.htm
/home/geneweb/gw/gw/setup/en/err_unkn.htm
/home/geneweb/gw/gw/setup/en/ged2gwb.htm
/home/geneweb/gw/gw/setup/en/gw2gd_ok.htm
/home/geneweb/gw/gw/setup/en/gwb2ged.htm
/home/geneweb/gw/gw/setup/en/gwc.htm
/home/geneweb/gw/gw/setup/en/gwd.htm
/home/geneweb/gw/gw/setup/en/gwd_info.htm
/home/geneweb/gw/gw/setup/en/gwd_ok.htm
/home/geneweb/gw/gw/setup/en/gwf.htm
/home/geneweb/gw/gw/setup/en/gwf_1.htm
/home/geneweb/gw/gw/setup/en/gwf_ok.htm
/home/geneweb/gw/gw/setup/en/gwu.htm
/home/geneweb/gw/gw/setup/en/gwu_ok.htm
/home/geneweb/gw/gw/setup/en/intro.txt
/home/geneweb/gw/gw/setup/en/list.htm
/home/geneweb/gw/gw/setup/en/main.htm
/home/geneweb/gw/gw/setup/en/note.htm
/home/geneweb/gw/gw/setup/en/recover.htm
/home/geneweb/gw/gw/setup/en/recover1.htm
/home/geneweb/gw/gw/setup/en/recover2.htm
/home/geneweb/gw/gw/setup/en/ren_ok.htm
/home/geneweb/gw/gw/setup/en/rename.htm
/home/geneweb/gw/gw/setup/en/save.htm
/home/geneweb/gw/gw/setup/en/simple.htm
/home/geneweb/gw/gw/setup/en/traces.htm
/home/geneweb/gw/gw/setup/en/welcome.htm
/home/geneweb/gw/gw/setup/es/backg.htm
/home/geneweb/gw/gw/setup/es/bsi.htm
/home/geneweb/gw/gw/setup/es/bsi_err.htm
/home/geneweb/gw/gw/setup/es/bso.htm
/home/geneweb/gw/gw/setup/es/bso_err.htm
/home/geneweb/gw/gw/setup/es/bso_ok.htm
/home/geneweb/gw/gw/setup/es/clean_ok.htm
/home/geneweb/gw/gw/setup/es/cleanup.htm
/home/geneweb/gw/gw/setup/es/cleanup1.htm
/home/geneweb/gw/gw/setup/es/consang.htm
/home/geneweb/gw/gw/setup/es/consg_ok.htm
/home/geneweb/gw/gw/setup/es/del_ok.htm
/home/geneweb/gw/gw/setup/es/delete.htm
/home/geneweb/gw/gw/setup/es/delete_1.htm
/home/geneweb/gw/gw/setup/es/err_acc.htm
/home/geneweb/gw/gw/setup/es/err_cnfl.htm
/home/geneweb/gw/gw/setup/es/err_miss.htm
/home/geneweb/gw/gw/setup/es/err_name.htm
/home/geneweb/gw/gw/setup/es/err_ndir.htm
/home/geneweb/gw/gw/setup/es/err_ngw.htm
/home/geneweb/gw/gw/setup/es/err_outd.htm
/home/geneweb/gw/gw/setup/es/err_reco.htm
/home/geneweb/gw/gw/setup/es/err_smdr.htm
/home/geneweb/gw/gw/setup/es/err_unkn.htm
/home/geneweb/gw/gw/setup/es/ged2gwb.htm
/home/geneweb/gw/gw/setup/es/gw2gd_ok.htm
/home/geneweb/gw/gw/setup/es/gwb2ged.htm
/home/geneweb/gw/gw/setup/es/gwc.htm
/home/geneweb/gw/gw/setup/es/gwd.htm
/home/geneweb/gw/gw/setup/es/gwd_info.htm
/home/geneweb/gw/gw/setup/es/gwd_ok.htm
/home/geneweb/gw/gw/setup/es/gwf.htm
/home/geneweb/gw/gw/setup/es/gwf_1.htm
/home/geneweb/gw/gw/setup/es/gwf_ok.htm
/home/geneweb/gw/gw/setup/es/gwu.htm
/home/geneweb/gw/gw/setup/es/gwu_ok.htm
/home/geneweb/gw/gw/setup/es/intro.txt
/home/geneweb/gw/gw/setup/es/list.htm
/home/geneweb/gw/gw/setup/es/main.htm
/home/geneweb/gw/gw/setup/es/note.htm
/home/geneweb/gw/gw/setup/es/recover.htm
/home/geneweb/gw/gw/setup/es/recover1.htm
/home/geneweb/gw/gw/setup/es/recover2.htm
/home/geneweb/gw/gw/setup/es/ren_ok.htm
/home/geneweb/gw/gw/setup/es/rename.htm
/home/geneweb/gw/gw/setup/es/save.htm
/home/geneweb/gw/gw/setup/es/simple.htm
/home/geneweb/gw/gw/setup/es/traces.htm
/home/geneweb/gw/gw/setup/es/welcome.htm
/home/geneweb/gw/gw/setup/fr/backg.htm
/home/geneweb/gw/gw/setup/fr/bsi.htm
/home/geneweb/gw/gw/setup/fr/bsi_err.htm
/home/geneweb/gw/gw/setup/fr/bso.htm
/home/geneweb/gw/gw/setup/fr/bso_err.htm
/home/geneweb/gw/gw/setup/fr/bso_ok.htm
/home/geneweb/gw/gw/setup/fr/clean_ok.htm
/home/geneweb/gw/gw/setup/fr/cleanup.htm
/home/geneweb/gw/gw/setup/fr/cleanup1.htm
/home/geneweb/gw/gw/setup/fr/consang.htm
/home/geneweb/gw/gw/setup/fr/consg_ok.htm
/home/geneweb/gw/gw/setup/fr/del_ok.htm
/home/geneweb/gw/gw/setup/fr/delete.htm
/home/geneweb/gw/gw/setup/fr/delete_1.htm
/home/geneweb/gw/gw/setup/fr/err_acc.htm
/home/geneweb/gw/gw/setup/fr/err_cnfl.htm
/home/geneweb/gw/gw/setup/fr/err_miss.htm
/home/geneweb/gw/gw/setup/fr/err_name.htm
/home/geneweb/gw/gw/setup/fr/err_ndir.htm
/home/geneweb/gw/gw/setup/fr/err_ngw.htm
/home/geneweb/gw/gw/setup/fr/err_outd.htm
/home/geneweb/gw/gw/setup/fr/err_reco.htm
/home/geneweb/gw/gw/setup/fr/err_smdr.htm
/home/geneweb/gw/gw/setup/fr/err_unkn.htm
/home/geneweb/gw/gw/setup/fr/ged2gwb.htm
/home/geneweb/gw/gw/setup/fr/gw2gd_ok.htm
/home/geneweb/gw/gw/setup/fr/gwb2ged.htm
/home/geneweb/gw/gw/setup/fr/gwc.htm
/home/geneweb/gw/gw/setup/fr/gwd.htm
/home/geneweb/gw/gw/setup/fr/gwd_info.htm
/home/geneweb/gw/gw/setup/fr/gwd_ok.htm
/home/geneweb/gw/gw/setup/fr/gwf.htm
/home/geneweb/gw/gw/setup/fr/gwf_1.htm
/home/geneweb/gw/gw/setup/fr/gwf_ok.htm
/home/geneweb/gw/gw/setup/fr/gwu.htm
/home/geneweb/gw/gw/setup/fr/gwu_ok.htm
/home/geneweb/gw/gw/setup/fr/intro.txt
/home/geneweb/gw/gw/setup/fr/list.htm
/home/geneweb/gw/gw/setup/fr/main.htm
/home/geneweb/gw/gw/setup/fr/note.htm
/home/geneweb/gw/gw/setup/fr/recover.htm
/home/geneweb/gw/gw/setup/fr/recover1.htm
/home/geneweb/gw/gw/setup/fr/recover2.htm
/home/geneweb/gw/gw/setup/fr/ren_ok.htm
/home/geneweb/gw/gw/setup/fr/rename.htm
/home/geneweb/gw/gw/setup/fr/save.htm
/home/geneweb/gw/gw/setup/fr/simple.htm
/home/geneweb/gw/gw/setup/fr/traces.htm
/home/geneweb/gw/gw/setup/fr/welcome.htm
/home/geneweb/gw/gw/setup/intro.txt
/home/geneweb/gw/gw/setup/se/backg.htm
/home/geneweb/gw/gw/setup/se/bsi.htm
/home/geneweb/gw/gw/setup/se/bsi_err.htm
/home/geneweb/gw/gw/setup/se/bso.htm
/home/geneweb/gw/gw/setup/se/bso_err.htm
/home/geneweb/gw/gw/setup/se/bso_ok.htm
/home/geneweb/gw/gw/setup/se/clean_ok.htm
/home/geneweb/gw/gw/setup/se/cleanup.htm
/home/geneweb/gw/gw/setup/se/cleanup1.htm
/home/geneweb/gw/gw/setup/se/consang.htm
/home/geneweb/gw/gw/setup/se/consg_ok.htm
/home/geneweb/gw/gw/setup/se/del_ok.htm
/home/geneweb/gw/gw/setup/se/delete.htm
/home/geneweb/gw/gw/setup/se/delete_1.htm
/home/geneweb/gw/gw/setup/se/err_acc.htm
/home/geneweb/gw/gw/setup/se/err_cnfl.htm
/home/geneweb/gw/gw/setup/se/err_miss.htm
/home/geneweb/gw/gw/setup/se/err_name.htm
/home/geneweb/gw/gw/setup/se/err_ndir.htm
/home/geneweb/gw/gw/setup/se/err_ngw.htm
/home/geneweb/gw/gw/setup/se/err_outd.htm
/home/geneweb/gw/gw/setup/se/err_reco.htm
/home/geneweb/gw/gw/setup/se/err_smdr.htm
/home/geneweb/gw/gw/setup/se/err_unkn.htm
/home/geneweb/gw/gw/setup/se/ged2gwb.htm
/home/geneweb/gw/gw/setup/se/gw2gd_ok.htm
/home/geneweb/gw/gw/setup/se/gwb2ged.htm
/home/geneweb/gw/gw/setup/se/gwc.htm
/home/geneweb/gw/gw/setup/se/gwd.htm
/home/geneweb/gw/gw/setup/se/gwd_info.htm
/home/geneweb/gw/gw/setup/se/gwd_ok.htm
/home/geneweb/gw/gw/setup/se/gwf.htm
/home/geneweb/gw/gw/setup/se/gwf_1.htm
/home/geneweb/gw/gw/setup/se/gwf_ok.htm
/home/geneweb/gw/gw/setup/se/gwu.htm
/home/geneweb/gw/gw/setup/se/gwu_ok.htm
/home/geneweb/gw/gw/setup/se/intro.txt
/home/geneweb/gw/gw/setup/se/list.htm
/home/geneweb/gw/gw/setup/se/main.htm
/home/geneweb/gw/gw/setup/se/note.htm
/home/geneweb/gw/gw/setup/se/recover.htm
/home/geneweb/gw/gw/setup/se/recover1.htm
/home/geneweb/gw/gw/setup/se/recover2.htm
/home/geneweb/gw/gw/setup/se/ren_ok.htm
/home/geneweb/gw/gw/setup/se/rename.htm
/home/geneweb/gw/gw/setup/se/save.htm
/home/geneweb/gw/gw/setup/se/simple.htm
/home/geneweb/gw/gw/setup/se/traces.htm
/home/geneweb/gw/gw/setup/se/welcome.htm
/home/geneweb/gw/gwd
/home/geneweb/gw/gwsetup
%defattr(-,root,root)
%doc doc/*

%changelog
* Sun Sep 6 1999 Daniel de Rauglaudre
Version 2.06-5
- updated file list

* Sun Sep 5 1999 Daniel de Rauglaudre
Version 2.06-4
Deleted option -r in useradd (Redhat specific)

* Sat Aug 14 1999 Dan Kegel <dank@alumni.caltech.edu>
Created.
