{ mkDerivation, base, bytestring, cereal, containers, directory
, extra, filepath, fsnotify, stdenv, strict
}:
mkDerivation {
  pname = "telescope-ds-file";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring cereal containers directory extra filepath fsnotify
    strict
  ];
  homepage = "https://github.com/jerbaroo/telescope";
  description = "File-based data source for the Haskell Telescope framework";
  license = stdenv.lib.licenses.bsd3;
}
