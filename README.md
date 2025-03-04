# GeneWeb

GeneWeb is an open source genealogy software written in OCaml. It comes
with a Web interface and can be used off-line or as a Web service.

[![build status](https://github.com/geneweb/geneweb/workflows/ci/badge.svg)](https://github.com/geneweb/geneweb/actions)

## Documentation

- Documentation maintained by the community: http://geneweb.tuxfamily.org/
- GeneWeb API (generated from source): http://geneweb.github.io/geneweb/
- GeneWeb overview (realized by OCamlPro): http://geneweb.github.io/

## Installation (for users)

WARNING: before installing a new version of GeneWeb, it is highly recommended to save
your bases into .gw formated files.

When installing a version of GeneWeb with the "pre-release" qualifier, you are
participating to the collective test effort (thanks for your contribution). You should keep aside the previous version
you were using, and refrain from extensive updates or additions in your bases
until the "release" qualifier is effective.

Any problem you encounter or issue you want to raise should be entered on the issue page
of the GitHub repository (https://github.com/geneweb/geneweb/issues)

Download the file corresponding to your environment from
the [releases page](https://github.com/geneweb/geneweb/releases).

Extract the distribution folder and place it at the location of your choice. You may alse rename it.
Its content is as follows (this example is for a Mac distribution.
Other distributions are very similar):
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

### Starting the GeneWeb servers

See the file `README.txt` of the distribution directory
([etc/README.txt](etc/README.txt)).

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

See [geneweb.opam](./geneweb.opam).

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
