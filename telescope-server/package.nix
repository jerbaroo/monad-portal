{ mkDerivation, base, servant-server, stdenv }:
mkDerivation {
  pname = "telescope-server";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base servant-server ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/jerbaroo/telescope";
  description = "A Servant server for the Haskell Telescope framework";
  license = stdenv.lib.licenses.bsd3;
}
