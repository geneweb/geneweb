# GeneWeb

GeneWeb is an open source genealogy software written in OCaml. It comes
with a Web interface and can be used off-line or as a Web service.

## This branch: master

This branch is the stable branch, i.e. that it may not contain the
last improvements.

## Documentation

The documentation is available online: http://geneweb.tuxfamily.org/

## Getting involved

We encourage you to participate in this open source project. We love
pull requests, bugs reports, ideas...

* Mailing list: https://groups.yahoo.com/neo/groups/GeneWeb/info
* IRC: irc://irc.freenode.net/geneweb
* Git: https://github.com/geneweb/geneweb
* Forum: http://www.geneanet.org/forum/GeneWeb-85
* Facebook group: http://www.facebook.com/geneweb
* Wikipedia: https://en.wikipedia.org/wiki/GeneWeb

## Building the code

1. Install OCaml (http://ocaml.org/)
  1. Install opam (https://opam.ocaml.org/)
  2. Install camlp5 (with opam)
  3. (Optional) Install ocamlfind (with opam)
2. Clone the repository
```
git clone https://github.com/geneweb/geneweb
```
3. Compil GeneWeb
```
./configure
make
make distrib
```

## Contributor guidelines

### Creating a pull request

Use the bug number/title as the name of pull request. Exemple of
commit message "PR #XXX: Title of the PR/Bug".

### Coding style

* Try to keep the same coding style as the existing one.
* New code should not contain any trailing whitespace.
* Each commit should have a single clear purpose. If a commit contains
  multiple unrelated changes, those changes should be split into
  separate commits.
* If a commit requires another commit to build properly, those commits
  should be squashed.
* If the PR needs to be update, push force.

## CONTENTS

|   File    |                  Description                            |
| --------- | ------------------------------------------------------- |
| CHANGES   | changes (for genealogists non programmers)              |
| ICHANGES  | changes (for programmers)                               |
| LICENSE   | license notice                                          |
| configure | configure script                                        |
| Makefile  | main Makefile                                           |
| README    | this file                                               |
| INSTALL   | installation file                                       |
| contrib   | users contributions                                     |
| dag2html  | library to create html trees                            |
| doc       | users’ html documentation                               |
| etc       | additional files for distribution                       |
| ged2gwb   | converter GEDCOM -> GeneWeb                             |
| gwb2ged   | converter GeneWeb -> GEDCOM                             |
| gwtp      | CGI to upload and download databases                    |
| launch    | a launcher of the geneweb server                        |
| hd        | html and image files for GeneWeb program                |
| rpm       | files to create Linux rpm packages                      |
| setup     | service (gwsetup) to launch commands in a Web navigator |
| src       | sources of main programs                                |
| tools     | tools for compiling                                     |
| wserver   | library for creating Web services                       |

## COPYRIGHT

All files marked in this distribution are Copyright (c) 1998-2016 INRIA
(Institut National de Recherche en Informatique et Automatique) and
distributed under the conditions stated in file LICENSE. They can be
freely redistributed for non-commercial purposes, provided the
copyright notice remains attached.

## INSTALLATION

You can compile on Unix, Windows or Mac OS X machines.
See the file INSTALL for installation instructions.
