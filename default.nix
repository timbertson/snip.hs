{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7102" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./nix/default.nix { }
