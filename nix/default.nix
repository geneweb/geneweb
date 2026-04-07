{ system, inputs }:
inputs.nixpkgs.lib.fixedPoints.composeManyExtensions [
  inputs.ocaml-ancient.outputs.overlays.${system}.default
  (super: self: {
    ocamlPackages = self.ocamlPackages.overrideScope (
      final: prev: {
        calendars = final.callPackage ./calendars.nix { };
        cmdliner = prev.cmdliner.overrideAttrs (rec {
          version = "2.1.0";
          src = self.fetchurl {
            url = "https://erratique.ch/software/cmdliner/releases/cmdliner-${version}.tbz";
            hash = "sha256-iBTGFM1D1S/R68ivWjHZElwhTEmPpgVmDk7Rlf+ENOk=";
          };
        });
        not-ocamlfind = final.callPackage ./not-ocamlfind.nix { };
        ocamlformat_0_29_0 = prev.ocamlformat.overrideAttrs (rec {
          version = "0.29.0";
          src = self.fetchurl {
            url = "https://github.com/ocaml-ppx/ocamlformat/releases/download/${version}/ocamlformat-${version}.tbz";
            sha256 = "sha256-2sd/CpV654K7S4abB7mAOocqNPjB6uiQG0LSG2I8nbU=";
          };
        });
        ocaml = prev.ocaml.override { framePointerSupport = true; };
        unidecode = final.callPackage ./unidecode.nix { };
        geneweb = final.callPackage ./geneweb.nix { };
        geneweb-compat = final.callPackage ./geneweb-compat.nix { };
        geneweb-http = final.callPackage ./geneweb-http.nix { };
        geneweb-rpc = final.callPackage ./geneweb-rpc.nix { };
      }
    );
  })
]
