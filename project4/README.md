# Automating dependency management

<meta charset="UTF-8">

This section covers how to automate dependency management so that people less
familiar with Nix can easily contribute to your project

Let's begin by considering a variation on the example from `project1`:

```nix
let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          foldl = haskellPackagesNew.callPackage ./nix/foldl.nix { };

          optparse-applicative =
            pkgs.haskell.lib.dontCheck
              (haskellPackagesNew.callPackage ./nix/optparse-applicative.nix { });

          project4 =
            haskellPackagesNew.callPackage ./nix/project4.nix { };

          turtle =
            pkgs.haskell.lib.doJailbreak
              (haskellPackagesNew.callPackage ./nix/turtle.nix { });
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { project4 = pkgs.haskellPackages.project4;
  }
```

Here we pin the versions of three dependencies: `optparse-applicative`, `foldl`,
and `turtle`.  Additionally, we disable tests for `optparse-applicative` (since
they fail) and jailbreak `turtle` because our new `foldl` dependency is out of
bounds for `turtle`.

This time we've moved all of our package derivations underneath the `nix/`
subdirectory so that we can take advantage of the handy `builtins.readDir`
function to automate some dependency boilerplate.

`release1.nix` shows an example of automating the addition of any Haskell
package that we store underneath the `nix/` subdirectory:

```nix
let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld:
          let
            toPackage = file: _: {
              name  = builtins.replaceStrings [ ".nix" ] [ "" ] file;

              value = haskellPackagesNew.callPackage (./. + "/nix/${file}") { };
            };

            packages = pkgs.lib.mapAttrs' toPackage (builtins.readDir ./nix);

          in
            packages // {
              optparse-applicative =
                pkgs.haskell.lib.dontCheck
                  (haskellPackagesNew.callPackage ./nix/optparse-applicative.nix { });

              turtle =
                pkgs.haskell.lib.doJailbreak
                  (haskellPackagesNew.callPackage ./nix/turtle.nix { });
            };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { project4 = pkgs.haskellPackages.project4;
  }
```

In the above code, the`packages` sub-expression expands out to:

```haskell
{ foldl =
    haskellPackagesNew.callPackage ./nix/foldl.nix { };

  optparse-applicative =
    haskellPackagesNew.callPackage ./nix/optparse-applicative.nix { };

  project4 =
    haskellPackagesNew.callPackage ./nix/project4.nix { };

  turtle =
    haskellPackagesNew.callPackage ./nix/turtle.nix { };
}
```

... which we then merge with the record of package-specific tweaks:

```nix
{ foldl =
    haskellPackagesNew.callPackage ./nix/foldl.nix { };

  optparse-applicative =
    haskellPackagesNew.callPackage ./nix/optparse-applicative.nix { };

  project4 =
    haskellPackagesNew.callPackage ./nix/project4.nix { };

  turtle =
    haskellPackagesNew.callPackage ./nix/turtle.nix { };
} // {
  optparse-applicative =
    pkgs.haskell.lib.dontCheck
      (haskellPackagesNew.callPackage ./nix/optparse-applicative.nix { });

  turtle =
    pkgs.haskell.lib.doJailbreak
      (haskellPackagesNew.callPackage ./nix/turtle.nix { });
};
```

The latter record takes precedence when merging the two, so the final result is:

```nix
{ foldl =
    haskellPackagesNew.callPackage ./nix/foldl.nix { };

  project4 =
    haskellPackagesNew.callPackage ./nix/project4.nix { };

  optparse-applicative =
    pkgs.haskell.lib.dontCheck
      (haskellPackagesNew.callPackage ./nix/optparse-applicative.nix { });

  turtle =
    pkgs.haskell.lib.doJailbreak
      (haskellPackagesNew.callPackage ./nix/turtle.nix { });
};
```

... which matches the set of overrides in our original `release0.nix`

```nix
let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld:
          let
            toPackage = file: _: {
              name  = builtins.replaceStrings [ ".nix" ] [ "" ] file;

              value = haskellPackagesNew.callPackage "./nix/${file}" { };
            };

            packages = pkgs.lib.mapAttrs toPackage (builtins.readDir ./nix);

          in
            builtins.listToAttrs packages // {
              optparse-applicative =
                pkgs.haskell.lib.dontCheck
                  (haskellPackagesNew.callPackage ./nix/optparse-applicative.nix { });

              turtle =
                pkgs.haskell.lib.doJailbreak
                  (haskellPackagesNew.callPackage ./nix/turtle.nix { });
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { project4 = pkgs.haskellPackages.project4;
  }
```

# Composing overrides

In the above example we had to throw away some of our auto-generated overrides
because we need to customize them.  For example, we need to jailbreak `turtle`,
so we have to repeat the import of the `./nix/turtle.nix` derivation.

However, we can get the best of both worlds by decomposing our Haskell overrides
into two separate sets of overrides:

* The first set of overrides we auto-generate from the `nix/` subdirectory
* In the second set of overrides we manually tweak Haskell packages

In order to do this, we take advantage of a useful utility called
`composeExtensions`.  As the name suggests, this lets us combine two extensions
into a single extension to our Haskell package overrides.

`release2.nix` demonstrates how this works:

```nix
let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages =
        let
          generatedOverrides = haskellPackagesNew: haskellPackagesOld:
            let
              toPackage = file: _: {
                name  = builtins.replaceStrings [ ".nix" ] [ "" ] file;

                value = haskellPackagesNew.callPackage (./. + "/nix/${file}") { };
              };

            in
              pkgs.lib.mapAttrs' toPackage (builtins.readDir ./nix);

          manualOverrides = haskellPackagesNew: haskellPackagesOld: {
            optparse-applicative =
              pkgs.haskell.lib.dontCheck
                haskellPackagesOld.optparse-applicative;

            turtle =
              pkgs.haskell.lib.doJailbreak
                haskellPackagesOld.turtle;
          };
        in
          pkgs.haskellPackages.override {
            overrides =
              pkgs.lib.composeExtensions generatedOverrides manualOverrides;
          };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { project4 = pkgs.haskellPackages.project4;
  }
```

Composing two extensions applies them in order: first we override the Haskell
derivations with the ones found in the `nix/` subdirectory and then we tweak
some of them by jailbreaking or disabling tests.

When we modify a derivation like `turtle` we now define the modification in
terms of `haskellPackagesOld`:

```haskell
  turtle = pkgs.haskell.lib.doJailbreak haskellPackagesOld.turtle;
```

This is because `haskellPackagesOld.turtle` now refers to our updated `turtle`
derivation from the preceding generated overrides.

We can take advantage of this trick to organize our overrides into sections:
one set of overrides for disabling tests, another set of overrides for
jailbreaking packages, and another set of overrides for jailbreaking packages.

The following `release3.nix` shows how to take advantage of Nix's automation
features to streamline such a configuration:

```nix
let
  # Disable tests for these packages
  dontCheckPackages = [
    "optparse-applicative"
  ];

  # Jailbreak these packages
  doJailbreakPackages = [
    "turtle"
  ];

  # Disable haddocks for these packages
  dontHaddockPackages = [
  ];

  config = {
    packageOverrides = pkgs: rec {
      haskellPackages =
        let
          generatedOverrides = haskellPackagesNew: haskellPackagesOld:
            let
              toPackage = file: _: {
                name  = builtins.replaceStrings [ ".nix" ] [ "" ] file;

                value = haskellPackagesNew.callPackage (./. + "/nix/${file}") { };
              };

            in
              pkgs.lib.mapAttrs' toPackage (builtins.readDir ./nix);

          makeOverrides =
            function: names: haskellPackagesNew: haskellPackagesOld:
              let
                toPackage = name: {
                  inherit name;

                  value = function haskellPackagesOld.${name};
                };

            in
              builtins.listToAttrs (map toPackage names);

          composeExtensionsList =
            pkgs.lib.fold pkgs.lib.composeExtensions (_: _: {});

          # More exotic overrides go here
          manualOverrides = haskellPackagesNew: haskellPackagesOld: {
          };
        in
          pkgs.haskellPackages.override {
            overrides = composeExtensionsList [
              generatedOverrides
              (makeOverrides pkgs.haskell.lib.dontCheck   dontCheckPackages  )
              (makeOverrides pkgs.haskell.lib.doJailbreak doJailbreakPackages)
              (makeOverrides pkgs.haskell.lib.dontHaddock dontHaddockPackages)
              manualOverrides
            ];
          };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { project4 = pkgs.haskellPackages.project4;
  }
```

This sort of automation simplifies the contribution for people less familiar
with Nix.  They can add new package versions by running:

```bash
$ cabal2nix ... > nix/${PACKAGE_NAME}.nix
```

... and they can control jailbreaking, tests, haddocks from simple lists at the
top of the configuration file.  Advanced users can still customize in more
detail inside the `manualOverrides` section.

The above example uses a handy `composeExtensionsList` function, which allows
you to easily add or subtract sets of extensions.  For example, you can turn
haddock generation back on for all packages by just commenting out the line for
haddock package overrides:

```nix
  overrides = composeExtensionsList [
    generatedOverrides
    (makeOverrides pkgs.haskell.lib.dontCheck   dontCheckPackages  )
    (makeOverrides pkgs.haskell.lib.doJailbreak doJailbreakPackages)
#   (makeOverrides pkgs.haskell.lib.dontHaddock dontHaddockPackages)
    manualOverrides
  ];
```

Similarly, you might require some overrides to get your Haskell IDE to work.
You can add such overrides to the list easily, like this:

```nix
  overrides = composeExtensionsList [
    generatedOverrides
    (makeOverrides pkgs.haskell.lib.dontCheck   dontCheckPackages  )
    (makeOverrides pkgs.haskell.lib.doJailbreak doJailbreakPackages)
    (makeOverrides pkgs.haskell.lib.dontHaddock dontHaddockPackages)
    (./import ~/myHaskellOverrides.nix)
    manualOverrides
  ];
```

Nix is a programming language, so you can easily create these domain-specific
languages for others to configure your Haskell build.

# Conclusion

This concludes the section on automating dependency management.  This is the
last section of the tutorial.
