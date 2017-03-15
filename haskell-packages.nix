{ pkgs ? (import ./../pkgs.nix) {} }:

rec {
  haskellPackages = pkgs.haskell.packages.ghc802.override{
    overrides =
      let overrideAttrs = package: newAttrs: package.override (args: args // {
              mkDerivation = expr: args.mkDerivation (expr // newAttrs);
            });

      in self: super: {
          servant-pandoc = overrideAttrs super.servant-pandoc {
            jailbreak = true;
          };

          # https://github.com/folsen/opaleye-gen/issues/8
          opaleye-gen = haskellPackageGen { doFilter = false; } (
            pkgs.fetchFromGitHub{
              owner = "folsen";
              repo = "opaleye-gen";
              rev = "14938df0081187539f23f8547fb1b7762e286ac3";
              sha256 = "1xapgyhkn71m0arb06rv5b1cncz5gv9lybi3q4yavs8zh4jbkbn7";
            }
          );

          # New versions for opaleye-gen
          product-profunctors = super.product-profunctors_0_8_0_3;
        };
      };

  # haskellPackageGen takes some options and a source location and generates a
  # derivation which builds the haskell package at that source location.
  haskellPackageGen = { doFilter ? true
                      , doHaddock ? true
                      , extraEnvPackages ? [] # Any extra packages to be made available in the developer shell only
                      }: src:
    let filteredSrc = builtins.filterSource (path: type:
          type != "unknown" &&
          (baseNameOf path == "dist" -> type != "directory")
        ) src;

        package = pkgs.runCommand "default.nix" {} ''
          ${pkgs.haskell.packages.ghc802.cabal2nix}/bin/cabal2nix \
            ${if doFilter then filteredSrc else src} \
            ${if doHaddock then "" else "--no-haddock"} \
            > $out
        '';

        drv = haskellPackages.callPackage package {};

        envWithExtras = pkgs.lib.overrideDerivation drv.env (attrs: {
          buildInputs = attrs.buildInputs ++ extraEnvPackages;
        });
    in drv // { env = envWithExtras; };
}
