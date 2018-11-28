let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          project2 =
            haskellPackagesNew.callPackage ./project2.nix {
              tar = pkgs.libtar;
            };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  rec {
    project2 = pkgs.haskellPackages.project2;

    test = pkgs.runCommand "check" {} ''
      touch example
      tar cf example.tar example
      ${project2}/bin/project2 | tee $out
    '';
  }
