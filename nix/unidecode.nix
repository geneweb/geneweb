{
  buildDunePackage,
}:

buildDunePackage (finalAttrs: {
  pname = "unidecode";
  version = "0.5.0";
  duneVersion = "3";

  src = fetchTarball {
    url = "https://github.com/geneweb/unidecode/archive/refs/tags/v${finalAttrs.version}.tar.gz";
    sha256 = "1jyggxm5mz52pnd0sp63d3b7c6rhba2s0vwawc3d4aqksa07qglp";
  };
})
