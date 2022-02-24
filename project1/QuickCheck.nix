{ mkDerivation, base, containers, lib, random, template-haskell
, test-framework, tf-random, transformers
}:
mkDerivation {
  pname = "QuickCheck";
  version = "2.8.2";
  sha256 = "98c64de1e2dbf801c54dcdcb8ddc33b3569e0da38b39d711ee6ac505769926aa";
  revision = "2";
  editedCabalFile = "12dk1bfiwvppk492g0dx0hr9r75hfmwp8n9mf1jzi6j1y2rbw5ha";
  libraryHaskellDepends = [
    base containers random template-haskell tf-random transformers
  ];
  testHaskellDepends = [
    base containers template-haskell test-framework
  ];
  homepage = "https://github.com/nick8325/quickcheck";
  description = "Automatic testing of Haskell programs";
  license = lib.licenses.bsd3;
}
