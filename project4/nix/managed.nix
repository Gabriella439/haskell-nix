{ mkDerivation, base, stdenv, transformers }:
mkDerivation {
  pname = "managed";
  version = "1.0.4";
  sha256 = "1nf38g8r9s8b37f6yw9w21b28pd6nvlxfl3gzfbn89m0zswrwv0v";
  libraryHaskellDepends = [ base transformers ];
  description = "A monad for managed values";
  license = stdenv.lib.licenses.bsd3;
}
