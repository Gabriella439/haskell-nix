{ compiler ? "ghc802" }:

let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
              optparse-applicative =
                pkgs.haskell.lib.addBuildDepend
                  (haskellPackagesNew.callPackage ./optparse-applicative-2.nix { })
                  haskellPackagesNew.semigroups;

              project1 =
                haskellPackagesNew.callPackage ./default.nix { };

              turtle =
                haskellPackagesNew.callPackage ./turtle-2.nix { };
            };
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { project1 = pkgs.haskell.packages.${compiler}.project1;
  }
