{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, filepath, hakyll, hxt, pandoc, stdenv
      , lens, time, time-locale-compat
      }:
      mkDerivation {
        pname = "blog";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        # Needed for OSX
        executableSystemDepends = [
          pkgs.darwin.apple_sdk.frameworks.Cocoa
        ];
        executableHaskellDepends = [
          base filepath hakyll hxt pandoc time time-locale-compat lens
        ];
        homepage = "http://mjhoy.com";
        description = "mjhoy.com website";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
