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

NOTE : Run the configure script to update the ocaml variables
(tools/Makefile.ocaml). You need to build ocaml-redis from source
(see link), and then update tools/Makefile.ocaml and change the
variable LREDIS to point to your own installation.

CONTENTS:

  api_request.ml        module for the requests
  api_server.ml         module for the configuration of the server
  Makefile              main Makefile
  README.md             this file
  service               all the modules that computes the requests' results
