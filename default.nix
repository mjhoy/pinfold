{ mkDerivation, base, bytestring, heist, lens
, MonadCatchIO-transformers, mtl, snap, snap-core
, snap-loader-static, snap-server, stdenv, text, time, xmlhtml
, snaplet-postgresql-simple, transformers, postgresql-simple
, hspec-snap, hspec, hspec-core
}:
mkDerivation {
  pname = "pinfold";
  version = "0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring heist lens MonadCatchIO-transformers mtl snap
    snap-core snap-loader-static snap-server text time xmlhtml
    snaplet-postgresql-simple transformers postgresql-simple
    hspec-snap hspec hspec-core
  ];
  description = "Website for Jim Pinfold";
  license = stdenv.lib.licenses.unfree;
}
