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
