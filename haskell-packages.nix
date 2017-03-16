{ pkgs ? (import ./../pkgs.nix) {} }:

rec {
  haskellPackages = pkgs.haskell.packages.ghc802.override {
    overrides =
      self: super: {
          # https://github.com/folsen/opaleye-gen/issues/8
          opaleye-gen = haskellPackageGen { doFilter = false; } (
            pkgs.fetchFromGitHub {
              owner = "folsen";
              repo = "opaleye-gen";
              rev = "14938df0081187539f23f8547fb1b7762e286ac3";
              sha256 = "1xapgyhkn71m0arb06rv5b1cncz5gv9lybi3q4yavs8zh4jbkbn7";
            }
          );

          servant-auth-swagger =
            let
              src = pkgs.fetchFromGitHub {
                owner = "plow-technologies";
                repo = "servant-auth";
                rev = "fba71585cd39bd16e86580d5320f20e486d5b05a";
                sha256 = "14ihr5cr7jrqwk9990m64jmw0h4gvrakffddlagm5bm85z0x6csr";
              };
            in pkgs.haskell.lib.doJailbreak (haskellPackageGen { doFilter = false; } "${src}/servant-auth-swagger");

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
