
Overview
========

How to start a Geneweb server
-----------------------------

Starting the server
~~~~~~~~~~~~~~~~~~~

Starting a Geneweb web server requires two tools:

- :code:`gwd`, the actual web server;
- :code:`setup`, the database setup portal.

These two tools are part of the main distribution and can be found on :code:`_build`.
If you built the project with :code:`make clean distrib`, two scripts are available on
the directory :code:`distribution/`. Keep in mind they are scripts wrapping the actual
binaries.

To start the server on the current directory::

  _build/install/default/bin/geneweb.gwd -hd <location_of_hd_dir>

This will start the main web server on port 2317.

To start the setup server on the current directory::

  _build/default/bin/setup/setup.exe -gd <location_of_gwsetup_dir>

This will start the setup server on port 2316.

Configuration
~~~~~~~~~~~~~

Geneweb can be configured by defining a `<base name>.gwb` file. An example is
available in the :code:`etc` directory.

Architecture of Geneweb
-----------------------

Geneweb is built from three main components:

- the storage of the genealogical trees, divided in a patch file and the actual data;
- the libraries, reading the data from the storage and writing the patch file
- the binaries and the web server, reading and writing the storage

.. image:: diagram.png

Storage architecture
~~~~~~~~~~~~~~~~~~~~

.. toctree::
   :maxdepth: 2

   database

Libraries
---------


- `Util`: a library with miscellaneous useful modules for manipulating
  base data types.

- `Def`: the definition of the main type definitions used in Geneweb trees.

- `Sosa`: describes a Sosa-Stradonitz numbering (known an
  Ahnentafel numbering), associating natural identifiers to individuals. This
  library is virtual and has three different implementations. One of these
  implementations is selected by the configuration script.

    * `Sosa_array` is one of the `sosa` implementations. It represents naturals
      as pair of integers stored in an array `a` such that
      `sosa = a.(0) + base * a(1)`) where `base` is a hardcoded constant.

    * `Sosa_num` is one of the `sosa` implementation based on the `Big_int`
      library.

    * `Sosa_zarith` is one of the `sosa` implementation based on the `Zarith`
      library.

- `Gwdb_driver`: describes the storage implementation. While it is
  virtual, it currently has only one implementation, `gwdb-legacy`. It is wrapped
  by the `gwdb` library that exports many tools for database updates.

- `Core`: core of Geneweb for calculating consanguinity between persons.

- `Geneweb_export` : provides a functor for defining Json convertors for Geneweb's
  datatypes.

- `Def_show`: defines formatters and `string` converters for Geneweb's datatypes.

- `Geneweb`: the main library. It contains several kinds of files, from
  utilitarian modules to HTML generation.

- `Gwb2gedLib`: defines a function for exporting a base to a GEDCOM file.

- `Wserver`: a light-weight web server, used by `gwd` and `setup`.

- `Gwd_lib`: defines additional modules for the `gwd` web server.

- `GwuLib`: defines useful functions for exporting a database to a `.gw` file.

The documentation of each module is available on the automatically generated
documentation of geneweb (`$ make doc`)


Binaries
~~~~~~~~

Official binaries
-----------------

Here are the binaries maintained by Geneweb:

- `Connex <../binaries/connex.html>`_: calculates connex components of a base;

- `Consang <../binaries/consang.html>`_: calculates the consanguinity level of individuals;

- `Ged2gwb <../binaries/ged2gwb.html>`_: imports a GEDCOM 5.5.1 file to a Geneweb base;

- `Gwb2ged <../binaries/gwb2ged.html>`_: exports a base to a GEDCOM 5.5.1 base;

- `Gwc <../binaries/gwc.html>`_: creates a new database;

- `Gwd <../binaries/gwd.html>`_: starts Geneweb's main web server which allows to interact with bases;

- `Gwdiff <../binaries/gwdiff.html>`_: targets differences between two databases;

- `Fixbase <../binaries/gwfixbase.html>`_: checks the consistency of a base and applies patches;

- `Gwgc <../binaries/gwgc.html>`_: removes unused entries in Geneweb's arrays;

- `Gwrepl <../binaries/gwrepl.html>`_: an OCaml interactive top level, useful for scripts;

- `Gwu <../binaries/gwu.html>`_: exports a base to a :code:`.gw` file;

- `Setup <../binaries/setup.html>`_: a web server for selecting and creating databases.

.. toctree::
   :maxdepth: 1
   :caption: For mor details about every binary, see:

   ../binaries/official_binaries

Web-server and plug-ins
-----------------------

The :code:`gwd` web server is customizable with plug-ins; code replacing
the original behaviour of the web server handling requests. They are
dynamically loaded by :code:`gwd` at its start and each base can be activated
through the :code:`.gwb` file (:code:`plugins=*` for activating all plug-ins,
otherwise :code:`plugins=p1,p2,...`).

A plug-in is composed of its code, a :code:`dune` file for building and a
:code:`META` file, containing some informations about the plug-in itself.

Here is an example of :code:`META` file:

```
version:1
maintainers:OCamlPro
depends:plugin1,plugin2
```

The only field taken into account by `gwd` is `depends` as it is used to
check there are no circular dependencies between plug-ins.
