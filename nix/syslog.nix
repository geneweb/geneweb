{ buildDunePackage
}:

buildDunePackage {
  pname = "syslog";
  version = "2.0.2";
  duneVersion = "2";

  src = fetchTarball {
    url = "https://github.com/geneanet/ocaml-syslog/archive/refs/tags/v2.0.2.tar.gz";
    sha256 = "0azpiq8k1ksx6kjy4i6z95jxr4g97s8zgnrwizhqggz12djcs9jv";
  };
}
