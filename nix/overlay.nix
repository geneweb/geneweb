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
      unidecode = final.callPackage ./unidecode.nix { };
      ocamlformat-lib = final.callPackage ./ocamlformat/ocamlformat-lib.nix { };
      ocamlformat = final.callPackage ./ocamlformat/ocamlformat.nix { };
      dead_code_analyzer = final.callPackage ./dead_code_analyzer.nix { };
      oui = prev.oui.overrideAttrs {
        version = "dev";
        src = super.fetchFromGitHub {
          owner = "Halbaroth";
          repo = "ocaml-universal-installer";
          rev = "infer-extension-output";
          hash = "sha256-lerlZRSDLW8pkp0UB8X2jMiJS3m60gZwdUAI0WMZPM4=";
        };
      };
    }
  );
}
