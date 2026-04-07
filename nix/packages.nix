{
  system,
  inputs,
  framePointerSupport ? false,
  ...
}:
let
  genewebOverlay = import ./overlay.nix { inherit system inputs framePointerSupport; };
in
import inputs.nixpkgs {
  inherit system;
  overlays = [ genewebOverlay ];
}
