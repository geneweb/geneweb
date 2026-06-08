{
  fetchFromGitHub,
  buildDunePackage
}:

buildDunePackage rec {
  pname = "dead_code_analyzer";
  version = "1.2.0";

  src = fetchFromGitHub {
    owner = "LexiFi";
    repo = pname;
    rev = version;
    sha256 = "sha256-mG4AlZXAY7xWwB5PFF5OPa2GB9bOuuFByuLKkmDASHs=";
  };
}
