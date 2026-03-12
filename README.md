# GeneWeb

GeneWeb is an open source genealogy software written in OCaml. It comes
with a Web interface and can be used off-line or as a Web service.

[![Build status](https://ci.appveyor.com/api/projects/status/5a5yk7jvxk332pxu/branch/master?svg=true)](https://ci.appveyor.com/project/geneweb/geneweb/branch/master)

## Documentation

GeneWeb documentation is available online: http://geneweb.tuxfamily.org/

## Installation (for users)

WARNING: before installing a new version of GeneWeb, it is highly recommended that you save
your bases as .gw formated files (read documentation for instructions). The .gw files are simple text files used to create new GeneWeb databases. When they are created, a log of detected errors (such as births after deaths, improbable ages for parents, etc) is created. It is suggested that these errors are checked and corrected before migrating your GeneWeb database to a newer version.

When installing a version of GeneWeb identified as a "pre-release", you are invited to 
participate in a collective effort to test and improve GeneWeb (thanks for your contribution). You should keep your current database as a .gw file (see above) and refrain from extensive updates or additions in your bases
until the new version of GeneWeb is identified as a "release" version.

Any problem you encounter or issue you want to raise should be entered on the issues page
of the GitHub repository (https://github.com/geneweb/geneweb/issues)

Download the file corresponding to your environment from
the [releases page] (https://github.com/geneweb/geneweb/releases).

Extract the distribution folder and place it at a location of your choice. You may also rename it.
Its contents are as follows (this example is for a Mac distribution, but other distributions are very similar):
```
drwxr-xr-x  13 Henri  staff     416 10 jul 08:34 .
drwxr-xr-x  44 Henri  staff    1408 10 jul 16:14 ..
-rw-r--r--   1 Henri  staff  160427 10 jul 08:34 CHANGES.txt
-rw-r--r--   1 Henri  staff   18007 10 jul 08:34 LICENSE.txt
-rw-r--r--   1 Henri  staff    1053 10 jul 08:34 LISEZMOI.txt
-rw-r--r--   1 Henri  staff     950 10 jul 08:34 README.txt
-rw-r--r--   1 Henri  staff   10780 10 jul 08:34 START.htm
drwxr-xr-x   2 Henri  staff      64 10 jul 08:34 bases
-rwxr-xr-x   1 Henri  staff    1632 10 jul 08:34 geneweb.command
drwxr-xr-x  21 Henri  staff     672 10 jul 08:34 gw
-rwxr-xr-x   1 Henri  staff      67 10 jul 08:34 gwd.command
-rwxr-xr-x   1 Henri  staff      71 10 jul 08:34 gwsetup.command
```

Instructions for starting the GeneWeb server may depend on your specific environment.

### Windows

TBD

### MacOS

Apple provides a security mechanism preventing users from executing application
which are not provided by authenticated developpers. Such applications cannot be started
by double-clicking on their icons.
Apple provides a two step mechanism circumventing this security:
* right-click on the application icon (```gwd``` and ```gwsetup```). This will pop-up a window
mentioning the security issue, and providing an "open" button. Click on this button to open
the application. Ignore the resulting messages as no parameters were provided.
* once ```gwd``` and ```gwsetup``` have been started in this fashion, they will be white-listed
on your machine and subsequent opens will succeed.

After white-listing ```gwd``` and ```gwsetup```, double click on the ```geneweb.command```
file which will launch both servers with appropriate parameters.
With the configuration provided in this launch command the bases are located in
the ```bases``` folder.
You may reorganize your folder structure (and launch command) as described in the
documentation at ```http://geneweb.tuxfamily.org/```

### Linux

Quite similar to the MacOS solution, without the security check.
```xxx.command``` files have an equivalent ```xxx.sh``` variant.

## Resources

* Documentation: http://geneweb.tuxfamily.org/wiki/GeneWeb
* Mailing list: https://framalistes.org/sympa/subscribe/geneweb
* IRC: irc://irc.freenode.net/geneweb
* Git: https://github.com/geneweb/geneweb
* Forum: http://www.geneanet.org/forum/GeneWeb-85
* Facebook group: http://www.facebook.com/geneweb
* Wikipedia: https://en.wikipedia.org/wiki/GeneWeb

## Contribute

See [Contributor guidelines](CONTRIBUTING.md).

### Installation (for developpers)

See [AppVeyor](appveyor.yml).

### Build instructions

1. Run the configuration script
   ```
   $ ocaml ./configure.ml
   ```
2. Build the distibution
   ```
   $ make clean distrib
   ```

You can have a description of available configuration options using
```
$ ocaml ./configure.ml --help
```

### Building the API

The API uses the Google Protocol Buffer to exchange information
encoded as pb, json, xml.

Install [protoc](https://github.com/protocolbuffers/protobuf#protocol-compiler-installation),
and these extra dependencies:

```
$ opam install ocurl piqi piqilib redis redis-sync yojson
```

Then, tell the configure script to enable API.

```
$ ocaml ./configure.ml --api
```

### Coding style

* Try to keep the same coding style as the existing one.
* New code should not contain any trailing whitespace.
* Each pull request should have a single clear purpose. If it containes
  multiple unrelated changes, those changes should be split into
  separate pull requests.
* If the PR needs to be update, push force.
* When submitting a new feature/function, write documentation and tests if it is relevant.

## Copyright

All files marked in this distribution are Copyright (c) 1998-2016 INRIA
(Institut National de Recherche en Informatique et Automatique) and
distributed under the GNU GENERAL PUBLIC LICENSE. See [LICENSE](LICENSE) file
for details.
