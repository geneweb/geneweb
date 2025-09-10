GeneWeb 7.01

Open the START.htm page

If you intend to open a base, you must have started the gwd program
by double clicking on the gwd.bat (Windows), gwd.command (macOS) or gwd (Linux) icon.
If you intend to manage or create a base,  you must have started the gwsetup program
by double clicking on the gwsetup.bat (Windows), gwsetup.command (macOS) or gwsetup (Linux) icon.

On macOS geneweb.command will kill previous versions, start both programs and minimises
the corresponding window.

Installation in CGI mode

Before executing install-cgi.sh, you must adjust some of your own configuration
in the install-cgi.sh script and in install-cgi/gwd.cgi:
- WEB-ROOT : the web root of your Apache server
- MY-BASES : your bases folder

     -----------------

See the file CHANGES in the present directory for the changes since
the previous versions.
Changes are also reported in the download documentation WiKi page
https://geneweb.tuxfamily.org/wiki/download

     -----------------

GeneWeb is Copyright (c) 1998-2023 INRIA.
Original creator Daniel de Rauglaudre

Remarks, suggestions, questions, bug reports to:
  https://github.com/geneweb/geneweb/issues
  or
  https://framalistes.org/sympa/subscribe/geneweb

Please specify the version number above, the system you use (Unix,
Windows, Mac) and if possible the commit number shown when positioning the
mouse over the version number on the welcome page.
