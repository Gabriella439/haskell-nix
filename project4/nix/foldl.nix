{ mkDerivation, base, bytestring, comonad, containers
, contravariant, criterion, hashable, lib, mwc-random, primitive
, profunctors, text, transformers, unordered-containers, vector
, vector-builder
}:
mkDerivation {
  pname = "foldl";
  version = "1.3.0";
  sha256 = "36fae19b478e5d6f8ea032cd3558e38c94e4c0733ab52066a65c5e1454c836e6";
  revision = "2";
  editedCabalFile = "179qb8nd02x199kjxb8zk19j1zlcf0lr160pjzhsvm3dgmdnm42h";
  libraryHaskellDepends = [
    base bytestring comonad containers contravariant hashable
    mwc-random primitive profunctors text transformers
    unordered-containers vector vector-builder
  ];
  benchmarkHaskellDepends = [ base criterion ];
  description = "Composable, streaming, and efficient left folds";
  license = lib.licenses.bsd3;
}
