{ mkDerivation, base, stdenv, tar }:
mkDerivation {
  pname = "project2";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ];
  executableSystemDepends = [ tar ];
  license = stdenv.lib.licenses.bsd3;
}
