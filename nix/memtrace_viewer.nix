{ ocamlPackages, fetchFromGitHub }:

ocamlPackages.buildDunePackage {
  pname = "memtrace_viewer";
  version = "v0.17.0";
  duneVersion = "3";

  src = fetchFromGitHub {
    owner = "janestreet";
    repo = "memtrace_viewer";
    rev = "v0.17";
    sha256 = "";
  };
};
