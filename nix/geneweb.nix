{
  lib,
  buildDunePackage,
  alcotest,
  ancient,
  brotli,
  cmdliner,
  crunch,
  pcre2,
  benchmark,
  calendars,
  dune-site,
  camlp5,
  camlp-streams,
  decompress,
  fmt,
  logs,
  logs-syslog,
  jingoo,
  markup,
  ounit,
  ppx_blob,
  ppx_deriving,
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
    camlp5
    crunch
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
