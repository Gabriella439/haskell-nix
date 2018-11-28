let
  config = rec {
    packageOverrides = pkgs: rec {
      docker-container-large = pkgs.dockerTools.buildImage {
        name = "project3-container";
        config.Cmd = [ "${haskellPackages.project3}/bin/project3" ];
      };

      docker-container-small = pkgs.dockerTools.buildImage {
        name = "project3-container";
        config.Cmd = [ "${haskellPackages.project3-minimal}/bin/project3" ];
      };

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
                  enableSharedExecutables = false;
                }
              );

          project3-minimal =
            pkgs.haskell.lib.overrideCabal
              ( pkgs.haskell.lib.justStaticExecutables
                  ( haskellPackagesNew.callPackage ./project3.nix {
                      tar = pkgs.libtar;
                    }
                  )
              )
              ( oldDerivation: {
                  testToolDepends = [ pkgs.libarchive ];
                }
              );
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; system = "x86_64-linux"; };

in
  { project3 = pkgs.haskellPackages.project3;

    project3-minimal = pkgs.haskellPackages.project3-minimal;

    docker-container-small = pkgs.docker-container-small;

    docker-container-large = pkgs.docker-container-large;
  }
