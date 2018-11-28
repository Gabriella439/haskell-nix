let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackgesOld: rec {
          project3 =
            pkgs.haskell.lib.overrideCabal
              ( haskellPackagesNew.callPackage ./project3.nix {
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
