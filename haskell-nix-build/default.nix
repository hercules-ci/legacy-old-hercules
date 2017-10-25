{ pkgs ? (import ../pkgs.nix) {} }:

with (import ../haskell-packages.nix) {inherit pkgs;};
with haskellPackages; 
with pkgs;
with (import ../.) {inherit pkgs;};

haskellPackageGen { 
  extraEnvPackages = [ ];
  extraAttrs = {
    HERCULES_NIX_VERSION = "${storeLib.nix}";
    HERCULES_STORE_LIB = "${storeLib}/lib/libhercules-store.so";
  };
} ./.
