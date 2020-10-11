{ mkDerivation, base, bytestring, cereal, containers, directory
, extra, filepath, fsnotify, HUnit, stdenv, strict, telescope
}:
mkDerivation {
  pname = "telescope-ds-file";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring cereal containers directory extra filepath fsnotify
    strict telescope
  ];
  executableHaskellDepends = [ base telescope ];
  testHaskellDepends = [ base HUnit telescope ];
  homepage = "https://github.com/jerbaroo/telescope";
  description = "File-based data source for the Haskell Telescope framework";
  license = stdenv.lib.licenses.bsd3;
}
