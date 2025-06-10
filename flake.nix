{
  description = "GeneWeb";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    ocaml-ancient.url = "github:OCamlPro/ocaml-ancient/v0.10.x";
  };

  outputs = { self, nixpkgs, flake-utils, ocaml-ancient, ... }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        ancient = ocaml-ancient.outputs.packages.${system}.default;
        fetchFromGitHub = pkgs.fetchFromGitHub;

        ocamlPackages = pkgs.ocaml-ng.ocamlPackages.overrideScope (final: super: {
          # Add frame pointers for better profiling with Perf.
          ocaml = super.ocaml.override { framePointerSupport = true; };
        });

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
      {
        packages = rec {
          default = geneweb;
          unidecode = ocamlPackages.callPackage ./nix/unidecode.nix { };
          calendars = ocamlPackages.callPackage ./nix/calendars.nix { };
          syslog = ocamlPackages.callPackage ./nix/syslog.nix { };
          not-ocamlfind =
            ocamlPackages.callPackage ./nix/not-ocamlfind.nix { inherit ocamlPackages; };

          geneweb = ocamlPackages.buildDunePackage {
            pname = "geneweb";
            version = "dev";
            duneVersion = "3";
            src = ./.;

            buildInputs = [
              not-ocamlfind
              ancient
              unidecode
              calendars
              syslog
            ] ++ (with pkgs; [
              bash
              gcc
              gnumake
              pkg-config
              m4
              pcre2
              gmp
              zlib
              bubblewrap
            ]) ++ (with ocamlPackages; [
              camlp5
              camlp-streams
              camlzip
              cppo
              fmt
              jingoo
              markup
              ounit
              ppx_blob
              ppx_deriving
              ppx_import
              stdlib-shims
              uri
              uucp
              uunf
              uutf
              yojson
              zarith
              digestif
              pcre2
              alcotest
              ojs
              js_of_ocaml
              js_of_ocaml-ppx
              tls
              lwt
              lwt_ppx
              httpun-ws
              httpun-lwt-unix
              promise_jsoo
              benchmark
              pp_loc
            ]);
          };
        };

        formatter = pkgs.nixpkgs-fmt;

        devShells.default = pkgs.mkShell {
          packages = [ ocamlWrapped ] ++ (with ocamlPackages; [
            findlib
            utop
            odoc
            ocaml-lsp
            ocamlformat_0_27_0
            patdiff
          ]);

          inputsFrom = [
            self.packages.${system}.geneweb
          ];
        };
      });
}
