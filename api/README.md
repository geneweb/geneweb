OVERVIEW:

GeneWeb is a genealogy software with a Web interface. It can be used
off-line or as a Web service.

This is an API which works just like GeneWeb (e.g. it's a web server).
The difference is that instead of sending HTML, it uses the Google
Protocol Buffer to exchange informations encoded as pb, json, xml.

DEPENDANCIES:

  OCaml
  camlp5
  lwt
  mysql
  ocamlfind
  ocurl
  piqi
  piqilib
  protobuf
  re
  uuidm
  yojson
  ocaml-redis (https://github.com/geneanet/ocaml-redis)

NOTE 1: While the issue about the Hashtbl is not fixed, use OCaml 3.12

NOTE 2: Don't forget to edit the Makefile (tools/Makefile and api/Makefile)
and change the values to mirror your installation. For example, change
CAMLP5D=~/.opam/system/lib/camlp5/ into CAMLP5D=~/.opam/3.12.1/lib/camlp5/

NOTE 3: Following the previous note, if you build ocaml-redis from sources,
change the value to point to your own installation (api/Makefile).

CONTENTS:

  api_request.ml        module for the requests
  api_server.ml         module for the configuration of the server
  Makefile              main Makefile
  README.md             this file
  package               deprecated (local dependancies)
  service               all the modules that computes the requests' results
  service/lib           deprecated (local dependancies)
