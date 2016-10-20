{ pkgs ? import <nixpkgs> {} }:

let
  haskellPackages = pkgs.haskell.packages.ghc801;
  f = import ./default.nix;
  drv = haskellPackages.callPackage f {};
in
  drv.env
