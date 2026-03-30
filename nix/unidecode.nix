{ buildDunePackage
}:

buildDunePackage (finalAttrs: {
  pname = "unidecode";
  version = "0.5.0";
  duneVersion = "3";

  src = fetchTarball {
    url = "https://github.com/geneweb/unidecode/archive/refs/tags/v${finalAttrs.version}.tar.gz";
    sha256 = "87ccf6a1da1aa7a8d3c0278940934df7c887a50d28485b668dea1d654d066631";
  };
})
