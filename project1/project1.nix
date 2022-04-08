{ mkDerivation, base, lib, turtle }:
mkDerivation {
  pname = "project1";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base turtle ];
  license = lib.licenses.bsd3;
}
