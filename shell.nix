{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, base64-bytestring, bytestring, filepath
      , hspec, hxt, QuickCheck, split, stdenv, text, vector, xmlgen, zlib
      }:
      mkDerivation {
        pname = "htiled";
        version = "0.1.4.0";
        src = ./.;
        libraryHaskellDepends = [
          base base64-bytestring bytestring filepath hxt split vector zlib
        ];
        testHaskellDepends = [
          base bytestring hspec hxt QuickCheck text xmlgen
        ];
        description = "Import from the Tiled map editor";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
