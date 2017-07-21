# Native dependencies

Nix can also provision non-Haskell dependencies for your project.  This project
provides an example of a small program that invokes `libtar` to open and close
a TAR file.

The first difference is that we have to extend the `cabal` file to add an
`extra-libraries` clause:

```cabal
name: project2
version: 1.0.0
license: BSD3
license-file: LICENSE
cabal-version: >= 1.18
build-type: Simple
extra-source-files: cbits

executable project2
    build-depends: base < 5
    default-language: Haskell2010
    c-sources: cbits/check.c
    extra-libraries: tar
    main-is: Main.hs
```

We also use the `c-sources` line so that Cabal can build and link in a small
test C program located at `cbits/check.c` for demonstration purposes.  This
program uses `libtar` to open and close a tar file named `example.tar`:

```c
#include <libtar.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>

void check() {
	int ret;
	TAR *t;

	ret = tar_open(&t, "example.tar", NULL, O_RDONLY, 0, TAR_VERBOSE);
	if (ret == -1) {
		fprintf(stderr, "tar_open failed\n");
		exit(1);
	}
	printf("Successfully opened example.tar\n");

	ret = tar_close(t);
	if (ret == -1) {
		fprintf(stderr, "tar_close failed\n");
		exit(1);
	}
	printf("Successfully closed example.tar\n");
}
```

... and then we refer to the `check` function from the Haskell code like this:

```haskell
module Main where

foreign import ccall "check" check :: IO ()

main :: IO ()
main = check
```

Like before, we transform the `project2.cabal` file into a Nix expression by
running `cabal2nix`:

```bash
$ cabal2nix . > default.nix
```

`cabal2nix` is smart and notices that our `project2.cabal` file has an
`extra-libraries` section and adds `tar` as a dependency of our package in
`default.nix`:

```nix
{ mkDerivation, base, stdenv, tar }:
mkDerivation {
  pname = "project2";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ];
  executableSystemDepends = [ tar ];
  license = stdenv.lib.licenses.bsd3;
}
```

The `tar` dependency goes in the `executableSystemDepends` section, which
indicates that this is a dependency on a native library and not a Haskell
dependency.

However, this alone is not enough to thread `libtar` to our package.  We need
to update our `release.nix` to specify which Nix package the `tar` dependency
comes from:

```nix
let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          project2 =
            haskellPackagesNew.callPackage ./default.nix {
              tar = pkgs.libtar;
            };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  rec {
    project2 = pkgs.haskellPackages.project2;

    test = pkgs.runCommand "check" {} ''
      touch example
      tar cf example.tar example
      ${project2}/bin/project2 | tee $out
    '';
  }
```

In fact, our `release.nix` file goes a step further and includes two attributes
instead of one:

* the `project2` attribute builds our project
* the `test` attribute runs a simple test to verify that the program works

If we only care about building the project, then we would just run:

```bash
$ nix-build --attr project2 release.nix
```

... but if we want to build and test our project, we can run:

```bash
$ nix-build --attr test release.nix
these derivations will be built:
  /nix/store/6z0jc3s2ya8j226rgma73w03pdphqwx8-project2-1.0.0.drv
  /nix/store/k82nnkiszzfcnjxq29gcbmgnh5cbfc51-check.drv
building path(s) ‘/nix/store/83bf1b1cp0qs6jgryb6yr0jfb759x4k5-project2-1.0.0’
...
configureFlags: --verbose --prefix=/nix/store/83bf1b1cp0qs6jgryb6yr0jfb759x4k5-project2-1.0.0 --libdir=$prefix/lib/$compiler --libsubdir=$pkgid --with-gcc=gcc --package-db=/tmp/nix-build-project2-1.0.0.drv-0/package.conf.d --ghc-option=-optl=-Wl,-rpath=/nix/store/83bf1b1cp0qs6jgryb6yr0jfb759x4k5-project2-1.0.0/lib/ghc-8.0.1/project2-1.0.0 --ghc-option=-j1 --enable-split-objs --disable-library-profiling --disable-profiling --enable-shared --enable-library-vanilla --enable-executable-dynamic --enable-tests --extra-include-dirs=/nix/store/pb3jxhy4z54i24i9s0kyszdmxd2xajc5-libtar-1.2.20/include --extra-lib-dirs=/nix/store/pb3jxhy4z54i24i9s0kyszdmxd2xajc5-libtar-1.2.20/lib
...
building path(s) ‘/nix/store/zc7ai4h44fydbm89a85b5bik5683klyk-check’
Successfully opened example.tar
Successfully closed example.tar
/nix/store/zc7ai4h44fydbm89a85b5bik5683klyk-check
```

Notice how the `configureFlags` passed to `project2` ensure that Nix's `libtar`
is on `cabal`'s search path when building the library.  Then the tests are run
and the output is saved to `/nix/store/zc7ai4h44fydbm89a85b5bik5683klyk-check`,
which we can also reference via the `result` symlink that `nix-build` creates:

```bash
$ cat result 
Successfully opened example.tar
Successfully closed example.tar
```

# Conclusion

This concludes managing native dependencies using Nix.  The next section covers
[customizing Haskell projects](../project3/README.md).
