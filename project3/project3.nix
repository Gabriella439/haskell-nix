{ mkDerivation, base, lib, tar, turtle }:
mkDerivation {
  pname = "project3";
  version = "1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  librarySystemDepends = [ tar ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base turtle ];
  license = lib.licenses.bsd3;
}
