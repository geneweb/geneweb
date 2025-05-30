{ buildDunePackage
}:

buildDunePackage {
  pname = "unidecode";
  version = "0.4.0";
  duneVersion = "3";

  src = fetchTarball {
    url = "https://github.com/geneweb/unidecode/archive/refs/tags/v0.4.0.tar.gz";
    sha256 = "1fahjzgr2zw8cplw5azd39z3m0dfv7wjbrzpb8953nadyzk87cjl";
  };
}
