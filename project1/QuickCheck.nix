{ mkDerivation, base, containers, random, stdenv, template-haskell
, test-framework, tf-random, transformers
}:
mkDerivation {
  pname = "QuickCheck";
  version = "2.8.2";
  sha256 = "1ai6k5v0bibaxq8xffcblc6rwmmk6gf8vjyd9p2h3y6vwbhlvilq";
  libraryHaskellDepends = [
    base containers random template-haskell tf-random transformers
  ];
  testHaskellDepends = [
    base containers template-haskell test-framework
  ];
  homepage = "https://github.com/nick8325/quickcheck";
  description = "Automatic testing of Haskell programs";
  license = stdenv.lib.licenses.bsd3;
}
