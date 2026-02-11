{
  description = "GeneWeb";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    ocaml-ancient = {
      url = "github:OCamlPro/ocaml-ancient/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, ocaml-ancient, ... }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          system = "${system}";
          overlays = [(_: prev: {
            ocamlPackages = prev.ocamlPackages.overrideScope (_: p: {
              cmdliner = prev.ocamlPackages.callPackage ./nix/cmdliner.nix { };
              unidecode = prev.ocamlPackages.callPackage ./nix/unidecode.nix { };
              calendars = prev.ocamlPackages.callPackage ./nix/calendars.nix { };
              not-ocamlfind = prev.ocamlPackages.callPackage ./nix/not-ocamlfind.nix { };
              odoc = prev.ocamlPackages.callPackage ./nix/odoc.nix { };
              ancient = ocaml-ancient.outputs.packages.${system}.ancient;
              # Produce a segment fault while compiling stdlib with jsoo...
              # ocaml = p.ocaml.override { framePointerSupport = true; };
            });
          })];
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
      {
        packages = rec {
          geneweb = ocamlPackages.buildDunePackage {
            pname = "geneweb";
            version = "dev";
            src = ./.;

            nativeBuildInputs = (with pkgs; [
              pkgs.git
            ]) ++ (with ocamlPackages; [
              not-ocamlfind
              cppo
              camlp5
            ]);

            propagatedBuildInputs = with ocamlPackages; [
              ancient
              pcre2
              benchmark
              calendars
              dune-site
              xdg
              camlp5
              camlp-streams
              decompress
              fmt
              logs
              logs-syslog
              jingoo
              markup
              ounit
              ppx_blob
              ppx_deriving
              ppx_import
              stdlib-shims
              unidecode
              uutf
              uunf
              uucp
              re
              uri
              yojson
              digestif
              pp_loc
            ];
          };

          geneweb-rpc = ocamlPackages.buildDunePackage {
            pname = "geneweb-rpc";
            version = "dev";
            src = ./.;

            buildInputs = [ geneweb ] ++ (with ocamlPackages; [
              lwt
              lwt_ppx
              tls-lwt
              cmdliner
              digestif
              httpun
              httpun-lwt-unix
              httpun-ws
              js_of_ocaml
              js_of_ocaml-ppx
              promise_jsoo
              benchmark
              pp_loc
              logs
              yojson
              fmt
            ]);
          };
        };

        formatter = pkgs.nixpkgs-fmt;

        devShells.default = pkgs.mkShell {
          packages = [ ocamlWrapped ] ++ (with ocamlPackages; [
            qcheck
            qcheck-alcotest
            findlib
            utop
            odoc
            ocaml-lsp
            ocamlformat_0_28_1
            patdiff
            memtrace
          ]);

          inputsFrom = [
            self.packages.${system}.geneweb
            self.packages.${system}.geneweb-rpc
          ];
        };
      });
}
