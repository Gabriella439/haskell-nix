{ mkDerivation, base, foldl, stdenv, text, turtle }:
mkDerivation {
  pname = "project4";
  version = "1.0.0";
  src = ../.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base foldl text turtle ];
  license = stdenv.lib.licenses.bsd3;
}
