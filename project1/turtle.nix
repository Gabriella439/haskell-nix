{ mkDerivation, ansi-wl-pprint, async, base, bytestring, clock
, criterion, directory, doctest, foldl, hostname, lib, managed
, optional-args, optparse-applicative, process, stm, system-fileio
, system-filepath, temporary, text, time, transformers, unix
, unix-compat
}:
mkDerivation {
  pname = "turtle";
  version = "1.3.2";
  sha256 = "f8163bcc08be06883964907391b88c2da0480f4764668629f4ada908319e7b5d";
  libraryHaskellDepends = [
    ansi-wl-pprint async base bytestring clock directory foldl hostname
    managed optional-args optparse-applicative process stm
    system-fileio system-filepath temporary text time transformers unix
    unix-compat
  ];
  testHaskellDepends = [ base doctest ];
  benchmarkHaskellDepends = [ base criterion text ];
  description = "Shell programming, Haskell-style";
  license = lib.licenses.bsd3;
}
