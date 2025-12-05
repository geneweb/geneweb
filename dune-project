(lang dune 3.6)

(name geneweb)
(generate_opam_files true)
(license GPL-2.0-only)
(authors "Daniel de Rauglaudre")
(maintainers "Elie Canonici-Merle <elie.canonicimerle@geneanet.org>")
(source (github geneweb/geneweb))
(homepage "https://geneweb.org")
(documentation "https://geneweb.tuxfamily.org/wiki/GeneWeb")

(package
  (name geneweb)
  (synopsis "Genealogy library and software")
  (description "GeneWeb is an open source genealogy software written in OCaml. It comes with a Web interface and can be used off-line or as a Web service.")
  (tags (genealogy))
  (depends
    (ocaml (>= 4.10))
    (alcotest :with-test)
    benchmark
    (calendars (= "1.0.0"))
    (camlp5 (>= "8.03.00"))
    camlp-streams
    cppo
    decompress
    fmt
    logs
    logs-syslog
    jingoo
    markup
    ounit
    ppx_blob
    ppx_deriving
    ppx_import
    stdlib-shims
    (unidecode (= "0.2.0"))
    uri
    uucp
    uunf
    uutf
    yojson
    digestif
    pp_loc
    dune-site
    (utop :with-dev-setup)
    (ocamlformat (and :with-dev-setup (= "0.28.1")))
  )
  (depopts
    (ancient (>= 0.10.0))
    zarith
  )
)

(package
  (name geneweb-rpc)
  (synopsis "RPC support for GeneWeb")
  (tags (geneweb rpc json))
  (depends
    (ocaml (>= 4.10))
    (geneweb (= :version))
    (qcheck :with-test)
    (qcheck-alcotest :with-test)
    lwt
    lwt_ppx
    tls
    (cmdliner (>= "2.0.0"))
    logs
    digestif
    httpun
    httpun-lwt-unix
    httpun-ws
    js_of_ocaml
    js_of_ocaml-ppx
    promise_jsoo
    yojson
    fmt))
