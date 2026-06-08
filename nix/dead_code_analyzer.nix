{
  fetchFromGitHub,
  buildDunePackage
}:

buildDunePackage {
  pname = "dead_code_analyzer";
  version = "1.2.0";
  
  src = fetchFromGitHub {
    owner = "LexiFi";
    repo = pname;
    rev = version;
    sha256 = "";
  };
}
