{ mkDerivation, aeson, base, bytestring, opaleye
, optparse-applicative, postgresql-simple, product-profunctors
, safe, servant-elm, servant-server, stdenv, text, wai, warp
}:
mkDerivation {
  pname = "hercules";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring opaleye postgresql-simple product-profunctors
    safe servant-server text wai warp
  ];
  executableHaskellDepends = [
    base bytestring optparse-applicative servant-elm
  ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/expipiplus1/hercules#readme";
  description = "A server to interface with a Hydra database";
  license = stdenv.lib.licenses.bsd3;
}
