{ buildDunePackage
}:

buildDunePackage {
  pname = "calendars";
  version = "1.0.0";
  duneVersion = "3";

  src = fetchTarball {
    url = "https://github.com/geneweb/calendars/archive/refs/tags/v1.0.0.tar.gz";
    sha256 = "11g4m8wzk1lww0nypd68jba6jvd2b4jpvj167sx734igqn7wnpxj";
  };
}
