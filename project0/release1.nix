let
  bootstrap = import <nixpkgs> { };

  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);

  src = bootstrap.fetchgit { inherit (nixpkgs) url rev sha256; };

  pkgs = import src { };

in
  pkgs.haskellPackages.callPackage ./default.nix { }
