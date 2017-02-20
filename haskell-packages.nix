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

          # https://github.com/plow-technologies/servant-auth/issues/25
          servant-auth-server = overrideAttrs super.servant-auth-server {
            version = "0.2.1.0";
            sha256 = "113c4s7ahm83g0931667rf5zrlq2199qnnn5j84dw5m9021q97hg";
            jailbreak = true;
            doCheck = false;
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

          #
          # New versions for opaleye-gen
          #
          product-profunctors = overrideAttrs super.product-profunctors {
            src = pkgs.fetchFromGitHub {
              owner = "tomjaguarpaw";
              repo = "product-profunctors";
              rev = "cd5071a285cbd436e1c8f4338befe846cfa5e1fb";
              sha256 = "1w6c82c83ab2irdmlqkv7ps391x73xi5j8s2cdv61832ijc6n3x5";
            };
          };
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
