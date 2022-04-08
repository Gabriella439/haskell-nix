{ mkDerivation, base, lib, tar }:
mkDerivation {
  pname = "project2";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ];
  executableSystemDepends = [ tar ];
  license = lib.licenses.bsd3;
}
