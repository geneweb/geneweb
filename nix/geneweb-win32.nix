{
  lib,
  buildDunePackage
}:

buildDunePackage {
  pname = "geneweb-win32";
  src = lib.cleanSource ../.;
  version = "dev";
  doCheck = true;
}
