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

  propagatedBuildInputs = [
    geneweb-win32
  ];

  buildInputs = [
    geneweb-compat
    camlp-streams
    logs
    fmt
  ];
}
