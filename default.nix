{ mkDerivation, aeson, base, opaleye, postgresql-simple
, product-profunctors, servant-server, stdenv, text, wai, warp
, postgresql
}:
mkDerivation {
  pname = "hercules";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base opaleye postgresql-simple product-profunctors
    servant-server text wai warp
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  buildDepends = [ postgresql ];
  homepage = "https://github.com/expipiplus1/hercules#readme";
  description = "A server to interface with a Hydra database";
  license = stdenv.lib.licenses.bsd3;
}
