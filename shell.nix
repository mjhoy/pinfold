{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, heist, lens
      , MonadCatchIO-transformers, mtl, snap, snap-core
      , snap-loader-static, snap-loader-dynamic, snap-server, stdenv, text, time, xmlhtml
      , snaplet-postgresql-simple, transformers, postgresql-simple
      , cabal-install
      }:
      mkDerivation {
        pname = "pinfold";
        version = "0.1";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base bytestring heist lens MonadCatchIO-transformers mtl snap
          snap-core snap-loader-static snap-loader-dynamic snap-server text time xmlhtml
          snaplet-postgresql-simple transformers postgresql-simple
          cabal-install
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
