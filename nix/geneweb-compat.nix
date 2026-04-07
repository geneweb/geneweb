{
  buildDunePackage,
}:

buildDunePackage {
  pname = "geneweb-compat";
  src = ../.;
  version = "dev";
}
