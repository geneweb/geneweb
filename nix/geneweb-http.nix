{
  lib,
  buildDunePackage,
  geneweb-compat,
  geneweb-win32,
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
    geneweb-win32
    camlp-streams
    logs
    fmt
  ];
}
