{ mkDerivation, ansi-wl-pprint, async, base, bytestring, clock
, directory, doctest, foldl, hostname, managed, optional-args
, optparse-applicative, process, stdenv, stm, system-fileio
, system-filepath, temporary, text, time, transformers, unix
, unix-compat
}:
mkDerivation {
  pname = "turtle";
  version = "1.3.2";
  sha256 = "0pbvkqqhiaddyhlqcrk48w7li81dijw92wwhchwqh1my1363n5pq";
  libraryHaskellDepends = [
    ansi-wl-pprint async base bytestring clock directory foldl hostname
    managed optional-args optparse-applicative process stm
    system-fileio system-filepath temporary text time transformers unix
    unix-compat
  ];
  testHaskellDepends = [ base doctest ];
  description = "Shell programming, Haskell-style";
  license = stdenv.lib.licenses.bsd3;
}
