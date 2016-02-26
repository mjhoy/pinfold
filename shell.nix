{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, heist, hspec, hspec-core
      , hspec-snap, lens, MonadCatchIO-transformers, mtl
      , postgresql-simple, readable, snap, snap-core, snap-loader-static
      , snap-loader-dynamic, snap-server, snaplet-postgresql-simple, snaplet-sass
      , stdenv , text, time, transformers, xmlhtml
      }:
      mkDerivation {
        pname = "pinfold";
        version = "0.1";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base bytestring heist lens MonadCatchIO-transformers mtl
          postgresql-simple readable snap snap-core snap-loader-static
          snap-loader-dynamic snap-server snaplet-postgresql-simple
          snaplet-sass text time transformers xmlhtml
        ];
        testHaskellDepends = [
          bytestring heist hspec hspec-core hspec-snap
          MonadCatchIO-transformers mtl postgresql-simple readable snap
          snap-core snap-loader-static snap-server snaplet-postgresql-simple
          snaplet-sass text time transformers xmlhtml
        ];
        description = "Website for Jim Pinfold";
        license = stdenv.lib.licenses.unfree;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
