let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld:
          let
            toPackage = file: _: {
              name  = builtins.replaceStrings [ ".nix" ] [ "" ] file;

              value = haskellPackagesNew.callPackage (./. + "/nix/${file}") { };
            };

            packages = pkgs.lib.mapAttrs' toPackage (builtins.readDir ./nix);

          in
            packages // {
              optparse-applicative =
                pkgs.haskell.lib.dontCheck
                  (haskellPackagesNew.callPackage ./nix/optparse-applicative.nix { });

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
