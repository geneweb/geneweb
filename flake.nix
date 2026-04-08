{
  description = "GeneWeb";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    flake-utils.url = "github:numtide/flake-utils";
    ocaml-ancient = {
      url = "github:OCamlPro/ocaml-ancient/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      ...
    }@inputs:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import ./nix/packages.nix { inherit system inputs; };
        ocamlPackages = pkgs.ocamlPackages;
      in
      {
        packages = {
          inherit (ocamlPackages)
            geneweb
            geneweb-compat
            geneweb-http
            geneweb-rpc
            ;
        };
        overlays.default = import ./nix/overlay.nix { inherit system inputs; };
        formatter = pkgs.nixfmt;
        devShells.default = import ./nix/devshell.nix { inherit system inputs; };
      }
    );
}
