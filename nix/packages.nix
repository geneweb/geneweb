{
  system,
  inputs,
  framePointerSupport ? false,
  overlays ? [ ],
  ...
}:
let
  genewebOverlay = import ./overlay.nix { inherit system inputs framePointerSupport; };
in
import inputs.nixpkgs {
  inherit system;
  overlays = overlays ++ [ genewebOverlay ];
}
