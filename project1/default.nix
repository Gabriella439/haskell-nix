{ mkDerivation, base, stdenv, turtle }:
mkDerivation {
  pname = "project1";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base turtle ];
  license = stdenv.lib.licenses.bsd3;
}
