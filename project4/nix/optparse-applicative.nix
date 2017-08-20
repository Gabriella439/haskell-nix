{ mkDerivation, ansi-wl-pprint, base, process, QuickCheck, stdenv
, transformers, transformers-compat
}:
mkDerivation {
  pname = "optparse-applicative";
  version = "0.13.0.0";
  sha256 = "1b0c5fdq8bd070g24vrjrwlq979r8dk8mys6aji9hy1l9pcv3inf";
  libraryHaskellDepends = [
    ansi-wl-pprint base process transformers transformers-compat
  ];
  testHaskellDepends = [ base QuickCheck ];
  homepage = "https://github.com/pcapriotti/optparse-applicative";
  description = "Utilities and combinators for parsing command line options";
  license = stdenv.lib.licenses.bsd3;
}
