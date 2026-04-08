{
  buildDunePackage,
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
