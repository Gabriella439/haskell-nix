{ mkDerivation, ansi-wl-pprint, async, base, bytestring, clock
, directory, doctest, fetchgit, foldl, hostname, managed
, optional-args, optparse-applicative, process, stdenv, stm
, system-fileio, system-filepath, temporary, text, time
, transformers, unix, unix-compat
}:
mkDerivation {
  pname = "turtle";
  version = "1.3.2";
  src = fetchgit {
    url = "https://github.com/Gabriel439/Haskell-Turtle-Library.git";
    sha256 = "0cbs3yi4glqhv3419hxihvpsgcj2h2sirbgym5d45hz4d32n9i67";
    rev = "21b50f09e04b4e149b3c5a5f12405ed608cda2ab";
  };
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
