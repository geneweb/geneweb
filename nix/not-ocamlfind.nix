{
  stdenv,
  fetchFromGitHub,
  ocaml,
  findlib,
  fmt,
  ocamlgraph,
  camlp-streams,
  rresult,
}:

stdenv.mkDerivation (finalAttrs: {
  name = "not-ocamlfind";
  version = "0.14";

  src = fetchFromGitHub {
    owner = "chetmurthy";
    repo = finalAttrs.name;
    rev = finalAttrs.version;
    sha256 = "5hw2oIgZGFVELVgja+vmRx+7vacnFaYDS5FKYe+87nY=";
  };

  buildInputs = [
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
      -config ${findlib}/etc/findlib.conf
  '';

  preInstall = ''
    mkdir -p $out/bin
    mkdir -p $out/lib/ocaml/${ocaml.version}/site-lib
  '';
})
