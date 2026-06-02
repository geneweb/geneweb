{
  lib,
  buildDunePackage,
  alcotest,
  ancient,
  brotli,
  pcre2,
  benchmark,
  calendars,
  dune-site,
  camlp5,
  camlp-streams,
  cmdliner,
  decompress,
  fmt,
  logs,
  logs-syslog,
  jingoo,
  markup,
  ounit,
  ppx_blob,
  ppx_deriving,
  ppx_import,
  qcheck,
  qcheck-alcotest,
  stdlib-shims,
  unidecode,
  uutf,
  uunf,
  uucp,
  re,
  uri,
  yojson,
  digestif,
  pp_loc,
  ptime,
  not-ocamlfind,
  cppo,
  geneweb-compat,
  geneweb-http,
}:

buildDunePackage {
  pname = "geneweb";
  version = "dev";
  src = lib.cleanSource ../.;
  doCheck = true;

  nativeBuildInputs = [
    brotli
    not-ocamlfind
    cppo
    camlp5
  ];

  buildInputs = [
    alcotest
    qcheck
    qcheck-alcotest
  ];

  propagatedBuildInputs = [
    geneweb-compat
    geneweb-http
    ancient
    cmdliner
    pcre2
    benchmark
    calendars
    dune-site
    camlp5
    camlp-streams
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
    unidecode
    uutf
    uunf
    uucp
    re
    uri
    yojson
    digestif
    pp_loc
    ptime
  ];
}
