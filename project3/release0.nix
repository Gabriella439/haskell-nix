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
