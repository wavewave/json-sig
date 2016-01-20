{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, filepath
      , haskell-src-exts, split, stdenv, text, unordered-containers
      , vector, cabal-install
      }:
      mkDerivation {
        pname = "json-sig";
        version = "0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson base bytestring haskell-src-exts split text
          unordered-containers vector
        ];
        executableHaskellDepends = [
          aeson base bytestring filepath haskell-src-exts text
          unordered-containers cabal-install
        ];
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
