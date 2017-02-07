{ pkgs ? (import ../pkgs.nix) {} }:

with (import ../haskell-packages.nix) {inherit pkgs;};
with haskellPackages; 
with pkgs;

haskellPackageGen { 
  extraEnvPackages = [ ];
} ./.
