let
  pkgs = import <nixpkgs> { };

in
  { project1 = pkgs.haskellPackages.callPackage ./default.nix { };
  }
