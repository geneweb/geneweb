self: super: {
  ocamlPackages = super.ocamlPackages.overrideScope (
    final: prev: {
      calendars = final.callPackage ./calendars.nix { };
      cmdliner = prev.cmdliner.overrideAttrs (rec {
        version = "2.1.0";
        src = super.fetchurl {
          url = "https://erratique.ch/software/cmdliner/releases/cmdliner-${version}.tbz";
          hash = "sha256-iBTGFM1D1S/R68ivWjHZElwhTEmPpgVmDk7Rlf+ENOk=";
        };
      });
      not-ocamlfind = final.callPackage ./not-ocamlfind.nix { };
      unidecode = final.callPackage ./unidecode.nix { };
      ocamlformat-lib = final.callPackage ./ocamlformat/ocamlformat-lib.nix { };
      ocamlformat = final.callPackage ./ocamlformat/ocamlformat.nix { };
      dead_code_analyzer = final.callPackage ./dead_code_analyzer.nix { };
      oui = prev.oui.overrideAttrs {
        version = "dev";
        src = super.fetchFromGitHub {
          owner = "Halbaroth";
          repo = "ocaml-universal-installer";
          rev = "4aec6b40aaf6c45ec82130fc029727c6cb074bd4";
          hash = "sha256-lr4+R2wOZTltUKceD4CLHhwbz+Mr2fMGwt2wJDPCY8w=";
        };
      };
    }
  );
}
