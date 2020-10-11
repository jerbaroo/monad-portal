{ mkDerivation, base, bytestring, cereal, containers, extra
, generics-eot, keys, stdenv, strict, zlib
}:
mkDerivation {
  pname = "telescope";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring cereal containers extra generics-eot keys strict
  ];
  homepage = "https://github.com/barischrooneyj/telescope";
  license = stdenv.lib.licenses.bsd3;
}
