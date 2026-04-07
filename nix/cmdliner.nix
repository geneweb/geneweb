{
  lib,
  stdenv,
  fetchurl,
  ocaml,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "cmdliner";
  version = "2.0.0";

  src = fetchurl {
    url = "https://erratique.ch/software/${finalAttrs.pname}/releases/${finalAttrs.pname}-${finalAttrs.version}.tbz";
    sha256 = "sha256-TlR6Yxw2+6rf9g0713JOs/g7onTpL7cllQuueGg3hYI=";
  };

  nativeBuildInputs = [ ocaml ];

  makeFlags = [ "PREFIX=$(out)" ];
  installTargets = "install install-doc";
  installFlags = [
    "LIBDIR=$(out)/lib/ocaml/${ocaml.version}/site-lib/${finalAttrs.pname}"
    "DOCDIR=$(out)/share/doc/${finalAttrs.pname}"
  ];

  postInstall = ''
    mv $out/lib/ocaml/${ocaml.version}/site-lib/${finalAttrs.pname}/{opam,${finalAttrs.pname}.opam}
  '';

  meta = with lib; {
    homepage = "https://erratique.ch/software/cmdliner";
    description = "OCaml module for the declarative definition of command line interfaces";
    license = licenses.isc;
    inherit (ocaml.meta) platforms;
    maintainers = [ maintainers.vbgl ];
  };
})
