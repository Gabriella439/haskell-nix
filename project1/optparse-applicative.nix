{ mkDerivation, ansi-wl-pprint, base, lib, process, QuickCheck
, transformers, transformers-compat
}:
mkDerivation {
  pname = "optparse-applicative";
  version = "0.13.0.0";
  sha256 = "cec6b1d94d347898a25446fb8a6643399d8429cf326f221e38a02d849b2b0cac";
  libraryHaskellDepends = [
    ansi-wl-pprint base process transformers transformers-compat
  ];
  testHaskellDepends = [ base QuickCheck ];
  homepage = "https://github.com/pcapriotti/optparse-applicative";
  description = "Utilities and combinators for parsing command line options";
  license = lib.licenses.bsd3;
}
