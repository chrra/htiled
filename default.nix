{ nixpkgs ? import <nixpkgs> {} }:
(nixpkgs.haskellPackages.override({
  overrides = self: super: {
    htiled = self.callPackage nix/default.nix {};
  };
})).htiled
