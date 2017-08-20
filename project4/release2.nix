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
