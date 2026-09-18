{
  buildDunePackage,
  benchmark,
  brotli,
  cmdliner,
  digestif,
  fmt,
  geneweb,
  geneweb-compat,
  httpun,
  httpun-lwt-unix,
  httpun-ws,
  js_of_ocaml,
  js_of_ocaml-ppx,
  logs,
  lwt,
  lwt_ppx,
  pp_loc,
  promise_jsoo,
  tls,
  yojson,
}:
buildDunePackage {
  pname = "geneweb-rpc";
  inherit (geneweb) version src;
  doCheck = true;

  nativeBuildInputs = [
    brotli
    cmdliner
    js_of_ocaml
  ];

  buildInputs = [
    benchmark
    cmdliner
    digestif
    fmt
    geneweb
    geneweb-compat
    httpun
    httpun-lwt-unix
    httpun-ws
    js_of_ocaml
    js_of_ocaml-ppx
    logs
    lwt
    lwt_ppx
    pp_loc
    promise_jsoo
    tls
    yojson
  ];
}
