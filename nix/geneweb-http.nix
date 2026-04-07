{
  buildDunePackage,
  geneweb-compat,
  camlp-streams,
  logs,
  fmt,
}:

buildDunePackage {
  pname = "geneweb-http";
  src = ../.;
  version = "dev";

  buildInputs = [
    geneweb-compat
    camlp-streams
    logs
    fmt
  ];
}
