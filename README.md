# Nix and Haskell in production

This guide documents how I use Nix for Haskell development.  Feel free to open
issues or pull requests if you would like to contribute or suggest improvements

The purpose of this project is to support two Haskell workflows:

* Workflow #1: Nix provisions the development environment
    * Nix provides all dependencies and the Haskell toolchain
    * You still build the root project using `cabal` ([overview](https://www.haskell.org/cabal/), [user's guide](https://cabal.readthedocs.io/))
    * This approach is ideal for development as it supports incremental builds
* Workflow #2: Nix builds the root project for you
    * This approach is ideal for continuous integration (especially Hydra)

The emphasis of this guide is to be as robust as possible and gracefully handle
writing Haskell projects at scale.  Some of the suggestions in this guide might
be overkill for a small Haskell project but are essential when managing multiple
private Haskell projects across a team of developers.

This guide is based partly on
[the Haskell section of the `nixpkgs` manual][nixpkgs-haskell]
and partly on experience using Nix and Haskell in production at
[Awake Security][awake].

# Background

Nix is not a `cabal` replacement and Nix actually complements `cabal` quite
well.  Nix is much more analogous to a `stack` replacement.  `stack` does
provide some support for Nix integration, but this document does not cover that.
Instead, this document describes how to use Nix in conjunction with `cabal` for
Haskell development

The main benefits of using Nix over `stack` are:

*   Binary caches

    Nix lets you download precompiled Hackage packages whereas `stack` compiles
    them on your computer the first time you depend on them

*   Space efficiency

    `stack` creates a copy of each package for each resolver.  This means that
    if you have two projects with different resolvers then they will not use
    the same copy of shared dependencies

*   Generality

    Nix is a language-independent build tool.  This means you can use Nix to
    also build and customize non-Haskell dependencies (like `gtk`).  This
    uniform language simplifies build tooling and infrastructure.

*   Larger ecosystem

    Nix provides a large ecosystem of tools that integrate with anything that
    Nix can build, such as Hydra (continuous integration), NixOS (an operating
    system), and NixOps (a deploy tool)

*   Flexibility

    Nix is a powerful tool in the hands of advanced users.  You can make very
    deep and sweeping changes to your toolchain, such as recompiling everything
    with security hardening

The main disadvantage of using Nix over `stack` are:

*   Verbosity

    Nix derivations for Haskell projects are significantly more complex than
    their corresponding `stack.yaml` files.  The `release.nix` files in this
    repository are the Nix analog of a `stack.yaml` file and you can see for
    yourself the increase in complexity as the examples progress in difficulty.

*   Poor error messages

    Nix is an untyped language with no special Haskell integration, so error
    messages are unhelpful

*   Nix cannot incrementally compile Haskell libraries

    Note that you can still use Nix to provision a development environment and
    incrementally compile a Haskell package using cabal. However, if you use Nix
    to build the package then Nix will build the package from scratch for every
    minor change. In theory, this could be fixed to have Nix directly support
    incremental Haskell builds but this has not been done yet.

*   Worse user experience

    Nix does not provide many conveniences that `stack` does such as
    bootstrapping new projects or "file watch"

Both Nix and `stack` use curated package sets instead of version bounds for
dependency management.  `stack` calls these package sets "resolvers" whereas
Nix calls these package sets "channels".  Nix provides stable channels with
names like `NixOS-18.09` (analogous to `stack`'s LTS releases) and then an
unstable channel named `nixpkgs-unstable` (analogous to `stack`'s nightly
releases)

# Related guides

* [Nix Haskell Monorepo Tutorial](https://github.com/fghibellini/nix-haskell-monorepo) -
  Guide on how to scale Nix development to a larger repository containing all of
  a company's internally-developed Haskell packages

# Related tools

Before continuing, I'd like to mention some other tools for mixing Haskell with
Nix:

* [`tinc`](https://github.com/sol/tinc/blob/nixpkgs/NIX.md) - this uses
  `cabal`'s solver to select which Haskell packages to use instead of the
  curated Haskell package set from `nixpkgs`
* [`styx`](https://github.com/jyp/styx) - This tool provides a `stack`-like
  interface to managing Haskell dependencies using Nix
* [`haskell-overridez`](https://github.com/adetokunbo/haskell-overridez) -
  Tool that automates dependency management as described in this guide

# Setup

Before you begin, you must install Nix if you haven't already:

```bash
$ curl -L https://nixos.org/nix/install | sh
```

You must also install `cabal2nix` and `nix-prefetch-git`:

```bash
$ nix-env --install cabal2nix
$ nix-env --install nix-prefetch-git
```

You also need to install `cabal` if you haven't done so already.  You can either
use your installed `cabal` or you can use `nix` to install `cabal` for you:

```bash
$ nix-env --install cabal-install
```

Make sure that you have a fairly recent version of `cabal` installed since these
examples will use GHC 8 which requires version 1.24 or later of `cabal`.  You
can check what version you have installed by running:

```bash
$ cabal --version
```

Finally, run `cabal update` if you haven't done so already

# Organization

This tutorial is split into several tutorial projects in the `project*/`
subdirectories.  Read the `README.md` file in each subdirectory in
order to follow the tutorial:

* [Project 0 - Nix basics][proj0]
* [Project 1 - Dependency management][proj1]
* [Project 2 - Non-Haskell dependencies][proj2]
* [Project 3 - Customizing Haskell projects ][proj3]
* [Project 4 - Advanced dependency management][proj4]

[awake]: https://awakesecurity.com/
[nixpkgs-haskell]: https://nixos.org/nixpkgs/manual/#haskell
[proj0]: ./project0/README.md
[proj1]: ./project1/README.md
[proj2]: ./project2/README.md
[proj3]: ./project3/README.md
[proj4]: ./project4/README.md
