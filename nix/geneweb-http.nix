{
  lib,
  buildDunePackage,
  geneweb-compat,
  camlp-streams,
  logs,
  fmt,
}:

buildDunePackage {
  pname = "geneweb-http";
  src = lib.cleanSource ../.;
  version = "dev";

  buildInputs = [
    geneweb-compat
    camlp-streams
    logs
    fmt
  ];
}
