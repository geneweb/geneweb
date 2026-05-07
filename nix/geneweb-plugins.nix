{
  lib,
  buildDunePackage,
  geneweb,
}:

buildDunePackage {
  pname = "geneweb-plugins";
  version = "dev";
  src = lib.cleanSource ../.;

  propagatedBuildInputs = [ geneweb ];
}
