{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc843" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, parallel, parsec, QuickCheck, stdenv
      , table-layout, text
      }:
      mkDerivation {
        pname = "tournament";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base parallel parsec QuickCheck table-layout text
        ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
