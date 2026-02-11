{ buildDunePackage
}:

buildDunePackage (finalAttrs: {
  pname = "calendars";
  version = "2.0.0";
  duneVersion = "3";

  src = fetchTarball {
    url = "https://github.com/geneweb/calendars/archive/refs/tags/v${finalAttrs.version}.tar.gz";
    sha256 = "169ar8178xqca3jc99zv0h1dg8q4bc5d3dfhfksqppc6zfpx0d3c";
  };
})
