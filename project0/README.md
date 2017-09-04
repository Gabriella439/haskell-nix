# Building with `nix`

 <meta charset="UTF-8">

This directory contains the simplest possible Haskell project and the simplest
possible Nix derivation to build that project.

You can build this Haskell package by running:

```bash
$ nix-build release0.nix
these derivations will be built:
  /nix/store/3l7f5v3aibz9rnxxafm6afvihjc04aiq-project0-1.0.0.drv
building path(s) ‘/nix/store/x28vx2rfnffl1clmxn5054bxwqyln2j0-project0-1.0.0’
...
Configuring project0-1.0.0...
Dependency base <5: using base-4.9.0.0
...
Building project0-1.0.0...
Preprocessing executable 'project0' for project0-1.0.0...
[1 of 1] Compiling Main             ( Main.hs, dist/build/project0/project0-tmp/Main.dyn_o )
Linking dist/build/project0/project0 ...
...
Installing executable(s) in
/nix/store/x28vx2rfnffl1clmxn5054bxwqyln2j0-project0-1.0.0/bin
...
/nix/store/x28vx2rfnffl1clmxn5054bxwqyln2j0-project0-1.0.0
```

I've highlighted the parts that matter the most for our simple example.  Don't
expect the hashed paths in the above example output to necessarily match the
ones you get (See below for more details about why they might differ).

The `project0.cabal` file only specifies a single dependency of `base < 5` and
Nix picks `base-4.9.0.0` to satisfy that dependency.  Nix then builds the
project, stores the build output in
`/nix/store/x28vx2rfnffl1clmxn5054bxwqyln2j0-project0-1.0.0` and creates a
symlink in the current directory named `result` pointing to that directory in
the `/nix/store`:

```
$ readlink result
/nix/store/x28vx2rfnffl1clmxn5054bxwqyln2j0-project0-1.0.0
```

Right now the contents of that directory are just an executable file and a
`LICENSE`:

```bash
$ tree result
result
├── bin
│   └── project0
└── share
    └── doc
        └── x86_64-linux-ghc-8.0.1
            └── project0-1.0.0
                └── LICENSE

5 directories, 2 files
```

As the project grows more complex we'll see additional build outputs in this
directory.

We can run the executable stored in the `result/bin` subdirectory:

```bash
$ result/bin/project0 
Hello, world!
```

This output makes sense since our `Main.hs` file is just a simple
"Hello, world!" program:

```haskell
module Main where

main :: IO ()
main = putStrLn "Hello, world!"
```

What happens if we try to build the project again?

```bash
$ nix-build release0.nix
these derivations will be built:
  /nix/store/vvw0v8ys7dadck747vj48vb0jgs7isqm-project0-1.0.0.drv
building path(s) ‘/nix/store/ysrqcpdl51jaa4gqzx1xmb7m0h05rdwq-project0-1.0.0’
setupCompilerEnvironmentPhase
...
[1 of 1] Compiling Main             ( Main.hs, dist/build/project0/project0-tmp/Main.dyn_o )
Linking dist/build/project0/project0 ...
...
/nix/store/ysrqcpdl51jaa4gqzx1xmb7m0h05rdwq-project0-1.0.0
```

This might seem odd at first since you'd expect Nix to reuse the cached result
from our first build.  However, Nix does not reuse the first build because our
project subtly changed: our `nix-build` deposited a `result` symlink which was
not there before!  Our Nix derivation depends on the current project directory,
so if anything in the current directory changes then Nix performs a complete
rebuild of our project.

We can verify this by removing the symlink and then performing the build again:

```bash
$ rm result
$ nix-build release0.nix
/nix/store/x28vx2rfnffl1clmxn5054bxwqyln2j0-project0-1.0.0
```

This time we get a cache hit and reuse the first build since our directory is
now bit-for-bit identical to when we first ran `nix-build`.

These wasteful rebuilds are one reason that I don't recommend using
`nix-build` to build the root Haskell project.  Instead, the next section
describes how to use `cabal` with Nix to avoid the issue of wasteful rebuilds.

# Building with `cabal`

You can open up a development environment for this project inside of a "Nix
shell" by running:

```bash
$ nix-shell --attr env release0.nix
```

Normally `nix-shell` wouldn't require the `--attr` flag since `nix-shell` is
designed to automatically compute the necessary development environment from
the original derivation.  However, Haskell derivations are different and
`nix-shell` doesn't work out of the box on them.  Haskell derivations are
records that provide a `env` field which `nix-shell` can use to compute the
development environment.  We pass the `--attr env` flag to specify that
`nix-shell` should compute the development environment from the derivation's
`env` "attribute".

Once we open up the development environment we can use `cabal` to build and run
the `project0` executable:

```bash
$ cabal configure
Resolving dependencies...
Configuring project0-1.0.0...
$ cabal run project0
Preprocessing executable 'project0' for project0-1.0.0...
[1 of 1] Compiling Main             ( Main.hs, dist/build/project0/project0-tmp/Main.o )
Linking dist/build/project0/project0 ...
Running project0...
Hello, world!
```

Unlike Nix, `cabal` will be smart and won't wastefully rebuild things that
haven't changed.  This means we can safely re-run `cabal` without rebuilding the
entire project from scratch:

```bash
$ cabal run project0
Preprocessing executable 'project0' for project0-1.0.0...
Running project0...
Hello, world!
```

You can exit from the Nix shell using the `exit` command or typing `Ctrl-D`.

This Nix shell provides all necessary dependencies for your project and the
Haskell toolchain *except for `cabal`*.  For example, inside the Nix shell you
will see that you are using a `ghc` provided by Nix, regardless of whether or
not you have a global `ghc` installed:

```bash
$ which ghc  # The exact hash in the path might differ
/nix/store/dg7ak1hvlj66vgn4fwvddnnr4pfncd04-ghc-8.0.1/bin/ghc
```

The `cabal configure` step automatically picks up the `ghc` tool-chain and
package database provisioned by Nix and uses them for all subsequent `cabal`
commands.

The `nixpkgs` manual notes that if you only have Haskell dependencies you
can also just run the following command once:

```
$ nix-shell --attr env release0.nix --run 'cabal configure'
```

... and then run all the other `cabal` commands without the Nix shell.  However,
if you have non-Haskell dependencies then this won't work.  When in doubt, just
get used to development inside of a Nix shell since that habit will translate
well to non-Haskell projects managed by Nix.

**NOTE:** I recommend using `cabal` to build the root project during Haskell
package development, but subsequent examples will still use `nix-build` to
keep the examples short.

# Nix derivations

The `release0.nix` file specifies how to build the project using Nix:

```nix
let
  pkgs = import <nixpkgs> { };

in
  pkgs.haskellPackages.callPackage ./default.nix { }
```

I don't recommend reusing the above derivation for your Haskell projects.  There
are several ways that we can improve upon this derivation that we'll address in
later examples.

This derivation begins by importing `nixpkgs`, which is a Nix channel.  You can
find all the officially supported Nix channels here:

* [Nix channels][channels]

The default Nix installation will subscribe you to the unstable Nix channel.
The only exception is if you are using NixOS as your operating system, in which
case you already have Nix installed and you will be subscribed to a stable Nix
channel by default.

You can tell which channel you have installed by running `nix-channel --list`.
For example, if you are on OS X, the output will probably say that you are
subscribed to the unstable channel:

```bash
$ nix-channel --list
nixpkgs https://nixos.org/channels/nixpkgs-unstable
```

... whereas on NixOS you probably be subscribed to a stable channel:

```bash
$ sudo nix-channel --list
nixos https://nixos.org/channels/nixos-16.09
```

You should probably use the default channel selected for you.  If you are using
a Linux operating system other than NixOS, you can safely change to a stable
channel if you prefer by running:

```bash
$ nix-channel --add https://nixos.org/channels/nixos-16.09-small nixpkgs
$ nix-channel --update nixpkgs
```

... replacing `16.09` with whatever stable release version you wish to use.

However, you should be very careful about using a stable release on OS X because
the public binary cache only caches OS X build products for the unstable
channel.  If you try to use a stable channel on OS X you run a very high risk of
compiling things from scratch (including `ghc`).

# Pinning `nixpkgs`

Even "stable" channels are still not frozen.  Stable channels are like major
releases and they periodically receive minor releases for security patches, bug
fixes, and new packages that successfully build.  However, you have to 
specifically opt in to channel updates by running
`nix-channel --update nixpkgs`.  If you do nothing then your channel will
remain frozen at whatever minor release you downloaded when you first installed
Nix.  When you do upgrade your channel you can only upgrade to the latest minor
release.

Nix's channel mechanism works okay for personal or open source development,
but does not work well in a corporate environment, since you can't easily
ensure that every person or deployment is on the exact same minor release.

In a corporate environment, you can pin `nixpkgs` to a specific `git` revision
as illustrated in `release1.nix`:

```nix
let
  bootstrap = import <nixpkgs> { };

  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);

  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    inherit (nixpkgs) rev sha256;
  };

  pkgs = import src { };

in
  pkgs.haskellPackages.callPackage ./default.nix { }
```

... where `nixpkgs.json` was generated using the `nix-prefetch-git` tool:

```bash
$ nix-prefetch-git https://github.com/NixOS/nixpkgs.git 2c288548b93b657365c27a0132a43ba0080870cc > nixpkgs.json
$ cat nixpkgs.json
{
  "url": "https://github.com/NixOS/nixpkgs.git",
  "rev": "2c288548b93b657365c27a0132a43ba0080870cc",
  "date": "2017-01-02T00:10:04+01:00",
  "sha256": "1a365am90a1zy99k4qwddj8s3bdlyfisrsq4a3r00kghjcz89zld"
}
```

Replace `2c288548b93b657365c27a0132a43ba0080870cc` with the `git` revision that
you want to pin `nixpkgs` to.  You can also omit the revision to pin to the
current `master`.

However, if you choose to go this route then you will need to set up an
internal Hydra server to build and cache your project.

Without an internal cache your developers will likely need to build these tools
from scratch whenever your pinned `nixpkgs` drifts too far from the publicly
cached channels.  `ghc` in particular is very expensive to rebuild.

This guide does not (yet) cover how to set up an internal Hydra server for this
purpose, but may do so in a future draft.  Until then, the remaining examples
will not use a pinned `nixpkgs` for simplicity.

[channels]: https://nixos.org/channels/

# `cabal2nix`

The second half of our `release0.nix` derivation contains the instructions to
build our project:

```nix
let
  pkgs = import <nixpkgs> { };

in
  pkgs.haskellPackages.callPackage ./default.nix { }
```

This references another file in this same project called `default.nix`.  This
file was generated using `cabal2nix` by running:

```bash
$ cabal2nix . > default.nix
```

... and the generated `default.nix` file for this project is:

```nix
{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "project0";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ];
  license = stdenv.lib.licenses.bsd3;
}
```

All that `cabal2nix` does is translate our `project0.cabal` file into a
corresponding Nix expression.  For comparison, here is the original
`project0.cabal` file that `default.nix` was generated from:

```cabal
name: project0
version: 1.0.0
license: BSD3
license-file: LICENSE
cabal-version: >= 1.18
build-type: Simple

executable project0
    build-depends: base < 5
    main-is: Main.hs
    default-language: Haskell2010
```

Any time you update a Haskell project's `cabal` file you need to regenerate the
`default.nix` file using `cabal2nix`.

# Hydra compatibility

`release2.nix` illustrates the next improvement we can make to our project's
Nix derivation:

```nix
let
  pkgs = import <nixpkgs> { };

in
  { project0 = pkgs.haskellPackages.callPackage ./default.nix { };
  }
```

The only difference is that now our file returns a "set" (the Nix term for a
dictionary) of derivations.  This "set" only has one "attribute" (i.e. key)
named `project0` whose value is the derivation to build our Haskell project.

The main motivation for this change is that Hydra (Nix's continuous integration
server) requires that project build files are sets of derivations with one
attribute per build product.  If you try to build a naked derivation with Hydra
you will get weird errors.

A second lesser reason for this change is that this makes it easy to add
additional build products to your project.  This comes in handy when you want to
build and test dependencies or extra tools that you rely on.

You can still build the project using `nix-build` either by specifying to build
all derivations in the set:

```bash
$ nix-build release2.nix
```

... or by specifying the attribute of the derivation you want to build using
the same `--attr` flag we introduced before for `nix-shell`:

```bash
$ nix-build --attr project0 release2.nix
```

This `--attr` flag specifies that we only want to build the `project0` field
of the record, and this flag comes in handy once the record has more than one
field.

You can also still open up a Nix shell, but you need to change the attribute you
pass on the command line from `env` to `project0.env`:

```bash
$ nix-shell --attr project0.env release2.nix
```

Like before, `nix-shell` and `nix-build` take slightly different attributes:
we specify the `project0` attribute when using `nix-build` and the
`project0.env` attribute when using `nix-shell`.

You can also avoid having to type this every time you initialize the project by
creating the following `shell.nix` file:

```nix
(import ./release2.nix).project0.env
```

... replacing `release2.nix` with the name of your project's derivation file.
Then you can just type:

```bash
$ nix-shell
```

... and that will automatically use the contents of `shell.nix`

Note that `cabal2nix` provides a `--shell` option to generate a `shell.nix`
file suitable for the current project.  However, this does not play nice with
advanced dependency management (covered in the next section) so I do not
recommend this approach in general.

# Conclusion

That concludes Nix workflow basics for Haskell development.  The
[next section](../project1/README.md) covers dependency management.
