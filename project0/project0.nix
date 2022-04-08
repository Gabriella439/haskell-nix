{ mkDerivation, base, lib }:
mkDerivation {
  pname = "project0";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ];
  license = lib.licenses.bsd3;
}
