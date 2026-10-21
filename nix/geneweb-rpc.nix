{
  buildDunePackage,
  brotli,
  lwt,
  lwt_ppx,
  tls-lwt,
  cmdliner,
  digestif,
  httpun,
  httpun-lwt-unix,
  httpun-ws,
  js_of_ocaml,
  js_of_ocaml-ppx,
  js_of_ocaml-compiler,
  promise_jsoo,
  benchmark,
  pp_loc,
  logs,
  yojson,
  fmt,
  geneweb-compat,
  geneweb,
}:
buildDunePackage {
  pname = "geneweb-rpc";
  inherit (geneweb) version src;
  doCheck = true;

  nativeBuildInputs = [
    brotli
    js_of_ocaml-compiler
  ];

  buildInputs = [
    geneweb-compat
    geneweb
    lwt
    lwt_ppx
    tls-lwt
    cmdliner
    digestif
    httpun
    httpun-lwt-unix
    httpun-ws
    js_of_ocaml
    js_of_ocaml-ppx
    promise_jsoo
    benchmark
    pp_loc
    logs
    yojson
    fmt
  ];
}
