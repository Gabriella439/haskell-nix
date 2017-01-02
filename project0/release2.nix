let
  pkgs = import <nixpkgs> { };

in
  { project0 = pkgs.haskellPackages.callPackage ./default.nix { };
  }
