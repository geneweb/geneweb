(lang dune 2.8)

(name geneweb)

(generate_opam_files true)
(license GPL-2.0-only)
(authors "Daniel de Rauglaudre")
(maintainers "Elie Canonici-Merle <elie.canonicimerle@geneanet.org>" "Olivier Pierre <olivier.pierre@geneanet.org>")
(source (github geneweb/geneweb))
(homepage "https://geneweb.org")
(documentation "https://geneweb.tuxfamily.org/wiki/GeneWeb")

(package
  (name geneweb)
  (synopsis "Genealogy library and software")
  (description "GeneWeb is an open source genealogy software written in OCaml. It comes with a Web interface and can be used off-line or as a Web service.")
  (tags (genealogy))
  (depends
    benchmark
    calendars
    (camlp5 (>= "8.00.01"))
    cppo
    (dune (>= "2.9.0"))
    jingoo
    markup
    num
    (ocaml (>= 4.08))
    odoc
    ounit
    ppx_blob
    ppx_deriving
    ppx_import
    stdlib-shims
    syslog
    (unidecode (= "0.2.0"))
    uucp
    uunf
    uutf
    zarith
  ))
