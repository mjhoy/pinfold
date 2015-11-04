{ mkDerivation, base, bytestring, heist, hspec, hspec-core
, hspec-snap, lens, MonadCatchIO-transformers, mtl
, postgresql-simple, snap, snap-core, snap-loader-static
, snap-server, snaplet-postgresql-simple, snaplet-sass, stdenv
, text, time, transformers, xmlhtml
}:
mkDerivation {
  pname = "pinfold";
  version = "0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring heist lens MonadCatchIO-transformers mtl
    postgresql-simple snap snap-core snap-loader-static snap-server
    snaplet-postgresql-simple snaplet-sass text time transformers
    xmlhtml
  ];
  testHaskellDepends = [
    bytestring heist hspec hspec-core hspec-snap
    MonadCatchIO-transformers mtl postgresql-simple snap snap-core
    snap-loader-static snap-server snaplet-postgresql-simple
    snaplet-sass text time transformers xmlhtml
  ];
  description = "Website for Jim Pinfold";
  license = stdenv.lib.licenses.unfree;
}
