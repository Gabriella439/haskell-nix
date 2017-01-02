# Exploring Haskell packages

 <meta charset="UTF-8">

This section covers how to tweak Haskell packages that Nix builds (typically
to fix failing builds).

Begin by running the following command to build the `morte` package:

```bash
$ nix-build '<nixpkgs>' -A haskellPackages.morte
...
/nix/store/z8rf74cylf9dqj63yb8p3j37sn8n49zf-morte-1.6.2
```

`nix-build` creates a `result` symlink pointing to the built package.  We can
use `tree` to study the directory layout of the built package:

```bash
$ tree result
result
├── bin
│   └── morte
├── lib
│   └── ghc-8.0.1
│       ├── morte-1.6.2
│       │   ├── libHSmorte-1.6.2-9FIpoXNPAFhL3LxBszO4VO.a
│       │   ├── libHSmorte-1.6.2-9FIpoXNPAFhL3LxBszO4VO-ghc8.0.1.so
│       │   └── Morte
│       │       ├── Context.dyn_hi
│       │       ├── Context.hi
│       │       ├── Core.dyn_hi
│       │       ├── Core.hi
│       │       ├── Import.dyn_hi
│       │       ├── Import.hi
│       │       ├── Lexer.dyn_hi
│       │       ├── Lexer.hi
│       │       ├── Parser.dyn_hi
│       │       ├── Parser.hi
│       │       ├── Tutorial.dyn_hi
│       │       └── Tutorial.hi
│       └── package.conf.d
│           └── morte-1.6.2-9FIpoXNPAFhL3LxBszO4VO.conf
├── nix-support
│   └── propagated-native-build-inputs
└── share
    ├── doc
    │   └── x86_64-linux-ghc-8.0.1
    │       └── morte-1.6.2
    │           ├── html
    │           │   ├── doc-index.html
    │           │   ├── frames.html
    │           │   ├── haddock-util.js
    │           │   ├── hslogo-16.png
    │           │   ├── index-frames.html
    │           │   ├── index.html
    │           │   ├── mini_Morte-Context.html
    │           │   ├── mini_Morte-Core.html
    │           │   ├── mini_Morte-Import.html
    │           │   ├── mini_Morte-Lexer.html
    │           │   ├── mini_Morte-Parser.html
    │           │   ├── mini_Morte-Tutorial.html
    │           │   ├── minus.gif
    │           │   ├── Morte-Context.html
    │           │   ├── Morte-Core.html
    │           │   ├── morte.haddock
    │           │   ├── Morte-Import.html
    │           │   ├── Morte-Lexer.html
    │           │   ├── Morte-Parser.html
    │           │   ├── Morte-Tutorial.html
    │           │   ├── morte.txt
    │           │   ├── ocean.css
    │           │   ├── plus.gif
    │           │   ├── src
    │           │   │   ├── hscolour.css
    │           │   │   ├── Morte-Context.html
    │           │   │   ├── Morte-Core.html
    │           │   │   ├── Morte-Import.html
    │           │   │   ├── Morte-Lexer.html
    │           │   │   ├── Morte-Parser.html
    │           │   │   └── Morte-Tutorial.html
    │           │   └── synopsis.png
    │           └── LICENSE
    └── x86_64-linux-ghc-8.0.1
        └── morte-1.6.2
            ├── bench
            │   └── src
            │       ├── concat.mt
            │       ├── factorial.mt
            │       └── recursive.mt
            └── test
                └── src
                    ├── example0.mt
                    ├── example10.mt
                    ├── example11.mt
                    ├── example12.mt
                    ├── example13.mt
                    ├── example14.mt
                    ├── example15.mt
                    ├── example1.mt
                    ├── example2.mt
                    ├── example3.mt
                    ├── example4.mt
                    ├── example5.mt
                    ├── example6.mt
                    ├── example7.mt
                    ├── example8.mt
                    └── example9.mt

19 directories, 68 files
```

This package has a significantly more complex layout:

*   `bin/`

    This directory stores any executable programs that the Haskell package
    produces.  Nix does not require that derivations deposit executables in this
    directory, but Nix does specially integrate with packages that do use this
    directory for their executables.  For example, if you use NixOS and you add
    a package to `environment.systemPackages` then the `bin/` directory for the
    package is added to the system `$PATH`.  Similarly, if you add a package as
    a `buildInput` to a Nix derivation, then any executables underneath `bin/`
    are added to the `$PATH` of the derivation's build script.

*   `lib/ghc-8.0.1/morte-1.6.2/`

    This contains all object code and interface files built by the Haskell
    library.  By default, Nix generates both a static library (i.e.
    `libHSmorte-1.6.2-9FIpoXNPAFhL3LxBszO4VO.a`) and a dynamic library
    (i.e. `libHSmorte-1.6.2-9FIpoXNPAFhL3LxBszO4VO-ghc8.0.1.so`) for any
    Haskell package that provides a `library` section in the `*.cabal` file

    By default, these libraries are compiled with GHC's `-fsplit-objs` flag to
    enable split object files, which reduces the library size but increases the
    compile time.  This is another reason why I recommend building the root
    project using `cabal` during development because by default `cabal` won't
    compile with split objects and will therefore build faster.  However,
    `-fsplit-objs` is a good default when Nix builds the project, so you don't
    need to change this setting.

    Nix also stores a "singleton" GHC package database holding just one package
    such as `package.conf.d/morte-1.6.2-9FIpoXNPAFhL3LxBszO4VO.conf`.  This is
    one difference between Nix and `stack`: `stack` uses up to three package
    databases (i.e. the global, snapshot, and project databases) whereas Nix
    uses a package database per package.  In practice this difference usually
    doesn't matter, but for more exotic build scenarios (such as compiling
    different sets of packages with different flags) this gives Nix greater
    flexibility than `stack`.

*   `nix-support/propagated-native-build-inputs`

    Anything which depends on the `morte` library must also depend on packages
    listed in `nix-support/propagated-native-build-inputs`.  This is how Nix
    ensures that you are not missing any libraries when linking packages.  For
    example, this is what the file looks like for `morte`:

    ```bash
    cat result/nix-support/propagated-native-build-inputs 
         /nix/store/ifgkzb0df0d6378399hny3ry8x530wim-Earley-0.11.0.1 /nix/store/3q914v2ijfj26fv5w9hd07a6lcnr4d89-http-client-0.4.31.1 /nix/store/xp55z3bpf92yaswyd6s9ajzr44w5m1r7-http-client-tls-0.2.4.1 /nix/store/39a8f70x5j09jnixj6cwv9l0n6jdncwb-microlens-0.4.7.0 /nix/store/rqd01fgj53wyc627wvns94j68a5iza07-microlens-mtl-0.1.10.0 /nix/store/x0g5n6k09f6zga000w9y0z1hx4i9ryyx-pipes-4.1.9 /nix/store/gkg8jgn8f8if6j83jhz3v6i5nnqwg8vc-system-fileio-0.3.16.3 /nix/store/x8chw4wp9r4dapsjmbgsbs3blgnmsxy5-system-filepath-0.4.13.4 /nix/store/zx0yg2d5jxwpdig83qica8hq4mv5l0hx-text-1.2.2.1 /nix/store/l4w7l07k9hr1qrmj9772zk60scw5xa4n-text-format-0.3.1.1   /nix/store/0crlsp45jgs2bs50bv68klpdz1b1jpk9-optparse-applicative-0.12.1.0 /nix/store/zx0yg2d5jxwpdig83qica8hq4mv5l0hx-text-1.2.2.1
    ```

    All of those paths are packages that `morte` depends on, each of which
    provides object code which must be linked into any executable that depends
    on `morte`

*   `share/doc/x86_64-linux-ghc-8.0.1/morte-1.6.2/html`

    These are the generated haddocks for the project.  You can open
    `share/doc/x86_64-linux-ghc-8.0.1/morte-1.6.2/html/index.html` in your
    browser to view `morte`'s documentation

*   `share/doc/x86_64-linux-ghc-8.0.1/morte-1.6.2/LICENSE`

    This is the project license

*   `x86_64-linux-ghc-8.0.1/morte-1.6.2`

    These are extra data files bundled with the project for running tests and
    benchmarks.  `cabal` includes these files because `morte` has the following
    line in the `morte.cabal` file:

    ```cabal
    Data-Files: bench/src/*.mt test/src/*.mt
    ```

# Tweaking Haskell projects

You can customize what Nix includes in a built Haskell package using the
`pkg.haskell.lib` suite of utilities.  For example, `release0.nix` contains an
example of disabling haddocks for the `morte` package:

```nix
let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          morte = pkgs.haskell.lib.dontHaddock haskellPackagesOld.morte;
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { morte = pkgs.haskellPackages.morte;
  }
```

You can find the full list of `pkgs.haskell.lib` utiliites in this file:

* [Utilities for tweaking Haskell packages][haskell-lib]

Some really convenient utilities include:

* `pkgs.haskell.lib.dontCheck` - Disable tests
* `pkgs.haskell.lib.appendPatch` - Patch a Haskell package
* `pkgs.haskell.lib.overrideCabal` - General tool for post-processing the output
  of `cabal2nix`

For example, this project is almost identical to the previous project except
that instead of using Nix to test the executable we now add a test suite to our
Haskell package that tests our TAR code in `Test.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative (empty)
import qualified Project3
import qualified Turtle

main :: IO ()
main = do
    Turtle.touch "example"
    Turtle.procs "bsdtar" ["cf", "example.tar", "example"] empty
    Project3.check
```

For whatever reason we use `bsdtar` instead of `tar` because we are gluttons for
punishment.

The `default.nix` file generated by `cabal2nix` has no idea that our test suite
depends on `bsdtar` so if we were to build and run the Haskell project without
any modifications the test suite would fail due to a missing `bsdtar`
executable.

However, our `release1.nix` file shows how we can tweak our project to include
`bsdtar` (provided by `libarchive`) for the test suite:

```nix
let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackgesOld: rec {
          project3 =
            pkgs.haskell.lib.overrideCabal
              ( haskellPackagesNew.callPackage ./default.nix {
                  tar = pkgs.libtar;
                }
              )
              ( oldDerivation: {
                  testToolDepends = [ pkgs.libarchive ];
                }
              );
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { project3 = pkgs.haskellPackages.project3;
  }
```

This uses the `pkgs.haskell.lib.overrideCabal` which is the most general
utility for tweaking derivations generated by `cabal2nix`.  You can find the
full set of fields that we can use to extend the `cabal2nix` code at the top of
this file:

* [Haskell generic builder][generic-builder]

These are also the same fields that you will see in an expression generated by
`cabal2nix`, such as `executableHaskellDepends` or `isLibrary`.  `overrideCabal`
lets you tweak any field you see in that list.

If you build and run the `release1.nix` project you will see that the test
suite runs and passes, indicating that our code works with TAR files generated
by `bsdtar`:

```bash
$ nix-build -A project3 release1.nix these derivations will be built:
  /nix/store/i7i38jx93qwxzwg9xakbd8lrfv9kbpi4-project3-1.0.0.drv
building path(s) ‘/nix/store/2z661k6rnwyiimympn1anmghdybi7izc-project3-1.0.0’
...
Building project3-1.0.0...
Preprocessing library project3-1.0.0...
[1 of 1] Compiling Project3         ( Project3.hs, dist/build/Project3.o )
Preprocessing executable 'project3' for project3-1.0.0...
[1 of 2] Compiling Project3         ( Project3.hs, dist/build/project3/project3-tmp/Project3.dyn_o )
[2 of 2] Compiling Main             ( Main.hs, dist/build/project3/project3-tmp/Main.dyn_o )
Linking dist/build/project3/project3 ...
Preprocessing test suite 'test' for project3-1.0.0...
[1 of 2] Compiling Project3         ( Project3.hs, dist/build/test/test-tmp/Project3.dyn_o )
[2 of 2] Compiling Main             ( Test.hs, dist/build/test/test-tmp/Main.dyn_o )
Linking dist/build/test/test ...
running tests
Running 1 test suites...
Test suite test: RUNNING...
Test suite test: PASS
Test suite logged to: dist/test/project3-1.0.0-test.log
1 of 1 test suites (1 of 1 test cases) passed.
...
/nix/store/2z661k6rnwyiimympn1anmghdybi7izc-project3-1.0.0
```

# Conclusion

This concludes the section on exploring and customizing Haskell packages.  This
is the last section of the tutorial

[haskell-lib]: https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/lib.nix
[generic-builder]: https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/generic-builder.nix
