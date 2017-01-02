# Note: This should fail to build
let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackgesOld: rec {
          project1 =
            haskellPackagesNew.callPackage ./default.nix { };

          turtle =
            haskellPackagesNew.callPackage ./turtle.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { project1 = pkgs.haskellPackages.project1;
  }
