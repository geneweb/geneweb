{
  lib,
  buildDunePackage,
}:

buildDunePackage {
  pname = "geneweb-compat";
  src = lib.cleanSource ../.;
  version = "dev";
}
