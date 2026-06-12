{
  description = "GeneWeb";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    flake-parts.url = "github:hercules-ci/flake-parts";
    ocaml-ancient = {
      url = "github:OCamlPro/ocaml-ancient";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { self, ... }@inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];

      perSystem =
        {
          pkgs,
          system,
          ...
        }:
        {
          _module.args.pkgs = import self.inputs.nixpkgs {
            inherit system;
            overlays = [
              inputs.ocaml-ancient.overlays.default
              (import ./nix/overlay.nix)
            ];
          };

          formatter = pkgs.nixfmt-tree;

          packages =
            let
              scope = pkgs.callPackage ./nix/packages.nix { };
            in
            {
              inherit (scope)
                geneweb-compat
                geneweb-http
                geneweb
                geneweb-rpc
                geneweb-plugins
                ;
            };

          apps.default = {
            type = "app";
            program = "${self.packages.${system}.geneweb}/bin/gwd";
            meta.description = "Run gwd server.";
          };

          devShells.default =
            let
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
              ++ (with ocamlPackages; [
                qcheck
                qcheck-alcotest
                alcotest
                findlib
                (odoc.overrideAttrs { doCheck = false; })
                ocaml-lsp
                patdiff
                memtrace
                ocamlformat
                oui
              ]);

              inputsFrom = [
                self.packages.${system}.geneweb-compat
                self.packages.${system}.geneweb-http
                self.packages.${system}.geneweb
                self.packages.${system}.geneweb-rpc
              ];
            };
        };
    };
}
