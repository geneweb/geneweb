{
  system,
  inputs,
  overlays ? [ ],
  ...
}:
let
  pkgs = import ./packages.nix {
    inherit system inputs overlays;
    framePointerSupport = true;
  };
  ocamlPackages = pkgs.ocamlPackages;
  # Due to Nix's package isolation principle, the findlib package cannot
  # install the topfind script into the OCaml directory. This wrapper
  # provides a workaround by adding the absolute path to this script to
  # directories searched by the OCaml compiler.
  ocamlWrapped = pkgs.symlinkJoin {
    name = "ocaml";
    paths = [ ocamlPackages.ocaml ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/ocaml \
        --add-flags "-I ${ocamlPackages.findlib}/lib/ocaml/${ocamlPackages.ocaml.version}/site-lib"
    '';
  };
in
pkgs.mkShell {
  packages = [
    ocamlWrapped
  ]
  ++ [
    pkgs.rlwrap
  ]
  ++ (with ocamlPackages; [
    qcheck
    qcheck-alcotest
    alcotest
    findlib
    utop
    ocaml-lsp
    ocamlformat_0_29_0
    patdiff
    memtrace
  ]);

  inputsFrom = with ocamlPackages; [
    geneweb
    geneweb-compat
    geneweb-http
    geneweb-rpc
  ];
}
