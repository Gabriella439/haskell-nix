{ mkDerivation, ansi-wl-pprint, async, base, bytestring, clock
, directory, doctest, fetchgit, foldl, hostname, managed
, optional-args, optparse-applicative, process, stdenv, stm
, system-fileio, system-filepath, temporary, text, time
, transformers, unix, unix-compat
}:
mkDerivation {
  pname = "turtle";
  version = "1.3.0";
  src = fetchgit {
    url = "https://github.com/Gabriel439/Haskell-Turtle-Library.git";
    sha256 = "1gib4m85xk7h8zdrxpi5sxnjd35l3xnprg9kdy3cflacdzfn9pak";
    rev = "ba9c992933ae625cef40a88ea16ee857d1b93e13";
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
