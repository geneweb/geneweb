{
  alcotest,
  ancient,
  benchmark,
  brotli,
  buildDunePackage,
  calendars,
  camlp-streams,
  camlp5,
  cmdliner,
  crunch,
  decompress,
  digestif,
  dune-site,
  fmt,
  geneweb-compat,
  geneweb-http,
  jingoo,
  lib,
  logs,
  logs-syslog,
  markup,
  not-ocamlfind,
  ounit,
  pcre2,
  pp_loc,
  ppx_blob,
  ppx_deriving,
  ptime,
  qcheck,
  qcheck-alcotest,
  re,
  stdlib-shims,
  unidecode,
  uri,
  uucp,
  uunf,
  uutf,
  yojson,
}:

buildDunePackage {
  pname = "geneweb";
  version = "dev";
  src = lib.cleanSource ../.;
  doCheck = true;

  nativeBuildInputs = [
    brotli
    camlp5
    crunch
    cmdliner
    not-ocamlfind
  ];

  buildInputs = [
    alcotest
    qcheck
    qcheck-alcotest
  ];

  propagatedBuildInputs = [
    ancient
    benchmark
    calendars
    camlp-streams
    camlp5
    cmdliner
    decompress
    digestif
    dune-site
    fmt
    geneweb-compat
    geneweb-http
    jingoo
    logs
    logs-syslog
    markup
    ounit
    pcre2
    pp_loc
    ppx_blob
    ppx_deriving
    ptime
    re
    stdlib-shims
    unidecode
    uri
    uucp
    uunf
    uutf
    yojson
  ];
}
