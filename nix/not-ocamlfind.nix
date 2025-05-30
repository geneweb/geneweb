{ stdenv
, ocamlPackages
, fetchFromGitHub
}:

stdenv.mkDerivation {
  name = "not-ocamlfind";
  version = "0.14";

  src = fetchFromGitHub {
    owner = "chetmurthy";
    repo = "not-ocamlfind";
    rev = "0.14";
    sha256 = "5hw2oIgZGFVELVgja+vmRx+7vacnFaYDS5FKYe+87nY=";
  };

  buildInputs = with ocamlPackages; [
    ocaml
    findlib
    fmt
    ocamlgraph
    camlp-streams
    rresult
  ];

  configurePhase = ''
    ./configure \
      -bindir $out/bin \
      -config ${ocamlPackages.findlib}/etc/findlib.conf
  '';

  preInstall = ''
    mkdir -p $out/bin
    mkdir -p $out/lib/ocaml/${ocamlPackages.ocaml.version}/site-lib
  '';
}
