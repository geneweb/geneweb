{
  lib,
  buildDunePackage,
  geneweb-compat,
  logs,
  fmt,
}:

buildDunePackage {
  pname = "geneweb-http";
  src = lib.cleanSource ../.;
  version = "dev";

  buildInputs = [
    geneweb-compat
    logs
    fmt
  ];
}
