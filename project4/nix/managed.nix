{ mkDerivation, base, lib, transformers }:
mkDerivation {
  pname = "managed";
  version = "1.0.4";
  sha256 = "1b6c9eb9fea0266497fb6f50d7e9b6a65d2456103c716fdc190be994d143c3d9";
  revision = "1";
  editedCabalFile = "1dhhh4j1yhx76wqzv6zbn7z1f6bfv1wps77drcr6290fc5ha1sg2";
  libraryHaskellDepends = [ base transformers ];
  description = "A monad for managed values";
  license = lib.licenses.bsd3;
}
