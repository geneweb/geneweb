{ lib
, stdenv
, zlib
, ocaml
, findlib
,
}:

stdenv.mkDerivation {
  pname = "ocaml${ocaml.version}-camlzip";
  version = "1.11";

  src = fetchTarball {
    url = "https://github.com/xavierleroy/camlzip/archive/rel111.tar.gz";
    sha256 = "16jnn3czxnvyjngnz167x5kw097k7izdqvkix8qvgvhdmgvqm89b";
  };

  nativeBuildInputs = [
    ocaml
    findlib
  ];

  propagatedBuildInputs = [ zlib ];

  strictDeps = true;

  createFindlibDestdir = true;

  postPatch = ''
    cp META-zip META-camlzip
    echo 'directory="../zip"' >> META-camlzip
    substituteInPlace Makefile \
      --subst-var-by ZLIB_LIBDIR "${zlib.out}/lib" \
      --subst-var-by ZLIB_INCLUDE "${zlib.dev}/include"
  '';

  buildFlags = [
    "all"
    "allopt"
  ];

  postInstall = ''
    ln -s $out/lib/ocaml/${ocaml.version}/site-lib/{,caml}zip
    mkdir $out/lib/ocaml/${ocaml.version}/site-lib/stublibs
    ln -s $out/lib/ocaml/${ocaml.version}/site-lib/{zip,stublibs}/dllcamlzip.so
  '';

  meta = with lib; {
    homepage = "http://cristal.inria.fr/~xleroy/software.html#camlzip";
    description = "Library for handling ZIP and GZIP files in OCaml";
    longDescription = ''
      This Objective Caml library provides easy access to compressed files in
      ZIP and GZIP format, as well as to Java JAR files.  It provides functions
      for reading from and writing to compressed files in these formats.
    '';
    license = "LGPL+linking exceptions";
    inherit (ocaml.meta) platforms;
    maintainers = with maintainers; [ maggesi ];
  };
}
