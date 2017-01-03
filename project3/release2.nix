let
  config = rec {
    packageOverrides = pkgs: rec {
      project3-minimal = pkgs.stdenv.mkDerivation {
        name = "project3-minimal";
        buildCommand = ''
          mkdir -p $out/bin
          cp ${haskellPackages.project3}/bin/project3 $out/bin/project3
        '';
      };

      docker-container-large = pkgs.dockerTools.buildImage {
        name = "project3-container";
        config.Cmd = [ "${haskellPackages.project3}/bin/project3" ];
      };

      docker-container-small = pkgs.dockerTools.buildImage {
        name = "project3-container";
        config.Cmd = [ "${project3-minimal}/bin/project3" ];
      };

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
                  enableSharedExecutables = false;
                }
              );
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { project3 = pkgs.haskellPackages.project3;

    project3-minimal = pkgs.project3-minimal;

    docker-container-small = pkgs.docker-container-small;

    docker-container-large = pkgs.docker-container-large;
  }
