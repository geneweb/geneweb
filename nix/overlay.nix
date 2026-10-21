self: super: {
  ocamlPackages = super.ocamlPackages.overrideScope (
    final: prev: {
      calendars = final.callPackage ./calendars.nix { };
      unidecode = final.callPackage ./unidecode.nix { };
      ocamlformat-lib = final.callPackage ./ocamlformat/ocamlformat-lib.nix { };
      ocamlformat = final.callPackage ./ocamlformat/ocamlformat.nix { };
      dead_code_analyzer = final.callPackage ./dead_code_analyzer.nix { };
      oui = prev.oui.overrideAttrs {
        version = "dev";
        src = super.fetchFromGitHub {
          owner = "OCamlPro";
          repo = "ocaml-universal-installer";
          rev = "44e8ec458dcc929300d39ad5b0332f24a8c4546d";
          hash = "sha256-XLI9n/04InhEmXMMv7at/ScUgDhJ8WWVcEeBJy7j1bE=";
        };
      };
      # mirage-crypto < 2.4.1 contains multiple vulnerabilites.
      mirage-crypto = prev.mirage-crypto.overrideAttrs (
        finalAttrs: _: {
          version = "2.4.1";

          src = super.fetchurl {
            url = "https://github.com/mirage/mirage-crypto/releases/download/v${finalAttrs.version}/mirage-crypto-${finalAttrs.version}.tbz";
            hash = "sha256-MyiGw2XGA1B3485namPaj3UAwVoWKLPAfhObDlUfH28=";
          };

          meta.knownVulnerabilities = [ ];
        }
      );
      mirage-crypto-ec = prev.mirage-crypto-ec.overrideAttrs ({
        meta.knownVulnerabilities = [ ];
      });
      mirage-crypto-pk = prev.mirage-crypto-pk.overrideAttrs ({
        meta.knownVulnerabilities = [ ];
      });
    }
  );
}
