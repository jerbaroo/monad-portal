{ mkDerivation, base, bytestring, cereal, containers, extra
, generics-eot, keys, stdenv, strict
}:
mkDerivation {
  pname = "telescope";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring cereal containers extra generics-eot keys strict
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/barischrooneyj/telescope";
  license = stdenv.lib.licenses.bsd3;
}
