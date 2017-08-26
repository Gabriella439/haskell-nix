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
