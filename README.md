# GeneWeb

[![build status](https://github.com/geneweb/geneweb/actions/workflows/ci.yml/badge.svg)](https://github.com/geneweb/geneweb/actions/workflows/ci.yml)

GeneWeb is an open source genealogy software written in OCaml. It comes
with a Web interface and can be used off-line or as a Web service.

## Documentation

- Documentation maintained by the community: https://geneweb.tuxfamily.org/
- GeneWeb API (generated from source): http://geneweb.github.io/geneweb/
- GeneWeb overview (realized by OCamlPro): https://geneweb.github.io/

## Quick and easy live GeneWeb test

- Test your GeneWeb database on current master: https://github.com/geneweb/geneweb/blob/master/geneweb_colab.ipynb

## Installation (for users)

WARNING: before installing a new version of GeneWeb, it is highly recommended to save
your bases into .gw formatted files.

When installing a version of GeneWeb with the "pre-release" qualifier, you are
participating to the collective test effort (thanks for your contribution). You should keep aside the previous version
you were using and refrain from extensive updates or additions in your bases
until the "release" qualifier is effective.

Any problem you encounter or issue you want to raise should be entered on the issue page
of the GitHub repository (https://github.com/geneweb/geneweb/issues).

Download the file corresponding to your environment from
the [releases page](https://github.com/geneweb/geneweb/releases).

Extract the distribution folder and place it at the location of your choice. You may also rename it.
Its content is as follows (this example is for a GNU/Linux distribution;
other distributions are very similar):

```
distribution/
├── bases
├── CHANGES.txt
├── gw
   ├── a.gwf
   ├── connex
   ├── consang
   ├── etc
   ├── ged2gwb
   ├── gwb2ged
   ├── gwc
   ├── gwd
   ├── gwd.arg
   ├── gwdiff
   ├── gwfixbase
   ├── gwsetup
   ├── gwu
   ├── images
   ├── lang
   ├── plugins
   ├── setup
   └── update_nldb
├── gwd.sh
├── gwsetup.sh
├── install-cgi
├── install-cgi.sh
├── LICENSE.txt
├── LISEZMOI.txt
├── README.txt
└── START.htm
```

Starting the GeneWeb servers may depend on your specific environment.

### Windows

TODO

### MacOS

Apple provides a security mechanism preventing users from executing applications
which are not provided by authenticated developers. Such applications cannot be started
by double-clicking on their icons.
Apple provides a two-step mechanism circumventing this security:
* Right-click on the application icon (```gwd``` and ```gwsetup```). This will pop-up a window
mentioning the security issue and providing an "open" button. Click on this button to open
the application. Ignore the resulting messages as no parameters were provided.
* Once ```gwd``` and ```gwsetup``` have been started in this fashion, they will be white-listed
on your machine and subsequent opens will succeed.

After white-listing ```gwd``` and ```gwsetup```, double-click on the ```geneweb.command```
file which will launch both servers with appropriate parameters.
With the configuration provided in this launch command, the bases are located in
the ```bases``` folder.
You may reorganize your folder structure (and launch command) as described in the
documentation at https://geneweb.tuxfamily.org/.

### Linux

Quite similar to the MacOS solution, without the security check.
```xxx.command``` files have an equivalent ```xxx.sh``` variant.

## Resources

* Documentation: https://geneweb.tuxfamily.org/wiki/GeneWeb
* Mailing list: https://framalistes.org/sympa/subscribe/geneweb
* IRC: irc://irc.libera.chat/geneweb
* Git: https://github.com/geneweb/geneweb
* Forum: https://www.geneanet.org/forum/GeneWeb-85
* Wikipedia: https://en.wikipedia.org/wiki/GeneWeb

## Contribute

See [Contributor guidelines](CONTRIBUTING.md).

### Installation (for developers)

See [geneweb.opam](./geneweb.opam).

### Build instructions

1. Run the configuration script
   ```
   $ ocaml ./configure.ml
   ```
2. Build the distribution
   ```
   $ make clean distrib
   ```

You can have a description of available configuration options using
```
$ ocaml ./configure.ml --help
```

## Copyright

All files marked in this distribution are Copyright (c) 1998-2016 INRIA
(Institut National de Recherche en Informatique et Automatique) and
distributed under the GNU GENERAL PUBLIC LICENSE. See [LICENSE](LICENSE) file
for details.
