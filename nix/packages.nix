{
  pkgs,
  ...
}:
pkgs.lib.makeScope pkgs.ocamlPackages.newScope (self: {
  geneweb-compat = self.callPackage ./geneweb-compat.nix { };
  geneweb-http = self.callPackage ./geneweb-http.nix { };
  geneweb = self.callPackage ./geneweb.nix { };
  geneweb-rpc = self.callPackage ./geneweb-rpc.nix { };
})
