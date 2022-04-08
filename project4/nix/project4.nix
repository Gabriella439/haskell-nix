{ mkDerivation, base, foldl, lib, text, turtle }:
mkDerivation {
  pname = "project4";
  version = "1.0.0";
  src = ../.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base foldl text turtle ];
  license = lib.licenses.bsd3;
}
