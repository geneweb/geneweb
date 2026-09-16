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
                geneweb-win32
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
            pkgs.mkShell {
              packages = with pkgs.ocamlPackages; [
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
                dead_code_analyzer
              ];

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
