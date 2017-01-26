{ pkgs ? (import ./../pkgs.nix) {} }:

let
  haskellPackages = pkgs.haskell.packages.ghc801.override{
    overrides =
      let overrideAttrs = package: newAttrs: package.override (args: args // {
              mkDerivation = expr: args.mkDerivation (expr // newAttrs);
            });

      in self: super: {
          servant-pandoc = overrideAttrs super.servant-pandoc {
            jailbreak = true;
          };

          cases = overrideAttrs super.cases {
            jailbreak = true;
          };

          pandoc = overrideAttrs super.pandoc {
            jailbreak = true;
          };

          postgresql-simple-migration = overrideAttrs super.postgresql-simple-migration {
            jailbreak = true;
            src = pkgs.fetchFromGitHub {
              owner = "ameingast";
              repo = "postgresql-simple-migration";
              rev = "5acb8fd57de13953fb665609b56845aadff37ea3";
              sha256 = "0wjlf8d3airlmdaalrfrmdaqm2r4diz55f76rh3i7xi9296cgc9h";
            };
          };

          servant-elm = overrideAttrs super.servant-elm {
            version = "2016-11-08";
            src = pkgs.fetchFromGitHub {
              owner = "mattjbray";
              repo = "servant-elm";
              rev = "07650488c990ead6483b5c9a5fde560705ad2f58";
              sha256 = "1q3bsvkv9kls8bi0jl36pjzla2kjvb6j3gla8f2wshxhash8ixvi";
            };
            libraryHaskellDepends = with self; [
              base elm-export lens servant servant-foreign text interpolate mockery wl-pprint-text
            ];
            doCheck = false;
          };

          #
          # New versions for servant-elm
          #
          elm-export = overrideAttrs super.elm-export {
            version = "2016-11-08";
            src = pkgs.fetchFromGitHub {
              owner = "krisajenkins";
              repo = "elm-export";
              rev = "d995e32482f9704efb5d7d08568b56c518f01bd0";
              sha256 = "0zy3m1bkg2jfcsr7ac57xk53wgpf5f44zy5rxhm2nmmq309jv7g7";
            };
          };

          # https://github.com/folsen/opaleye-gen/issues/8
          opaleye-gen = self.callPackage (
            haskellPackageGen { doFilter = false; } (
              pkgs.fetchFromGitHub{
                owner = "folsen";
                repo = "opaleye-gen";
                rev = "14938df0081187539f23f8547fb1b7762e286ac3";
                sha256 = "1xapgyhkn71m0arb06rv5b1cncz5gv9lybi3q4yavs8zh4jbkbn7";
              }
            )
          ) {};

          #
          # New versions for opaleye-gen
          #
          product-profunctors = overrideAttrs super.product-profunctors {
            src = pkgs.fetchFromGitHub {
              owner = "tomjaguarpaw";
              repo = "product-profunctors";
              rev = "1f14ce3f495cfaac292a342e9d67c3f2f753c914";
              sha256 = "0qchq0hky05w52wpz1b6rp3y7k6z3rs16kpab175j28nqf6h47f3";
            };
          };
        };
      };

  haskellPackageGen = { doHaddock ? false, doFilter ? true }: src:
    let filteredSrc = builtins.filterSource (n: t: t != "unknown") src;
        package = pkgs.runCommand "default.nix" {} ''
          ${pkgs.haskell.packages.ghc801.cabal2nix}/bin/cabal2nix \
            ${if doFilter then filteredSrc else src} \
            ${if doHaddock
                then ""
                else "--no-haddock"} \
            > $out
        '';
    in import package;
  f = haskellPackageGen {} ./.;

  drv = haskellPackages.callPackage f {};

  extraEnvPackages = with pkgs; with haskellPackages; [ opaleye-gen postgresql ];

  envWithExtras = pkgs.lib.overrideDerivation drv.env (attrs: {
    buildInputs = attrs.buildInputs ++ extraEnvPackages;
  });

in drv // { env = envWithExtras; }
