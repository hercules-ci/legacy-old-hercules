{ nixpkgs ? fetchTarball https://github.com/NixOS/nixpkgs/archive/9f251e1cb138f4a299b1da4257ba119311373ff4.tar.gz }:

let
  pkgs = import nixpkgs {};
  haskellPackages = pkgs.haskell.packages.ghc801.override{
    overrides =
      let overrideAttrs = package: newAttrs: package.override (args: args // {
              mkDerivation = expr: args.mkDerivation (expr // newAttrs);
            });
          servant-auth-src = pkgs.fetchFromGitHub {
            owner = "plow-technologies";
            repo = "servant-auth";
            rev = "e37c78c153048f0e0a8518645aa76a34d2d408b4";
            sha256 = "16c6n36wawz25q3kzfs1lq5wp0aj9vdz2algnfpc3rdpg36ynwwx";
          };

      in self: super: {
          servant-auth = self.callPackage (
            haskellPackageGen { doFilter = false; }
                              (servant-auth-src + "/servant-auth")
          ) {};

          servant-auth-server = self.callPackage (
            haskellPackageGen { doFilter = false; }
                              (servant-auth-src + "/servant-auth-server")
          ) {};

          servant-elm = overrideAttrs super.servant-elm {
            version = "2016-11-08";
            src = pkgs.fetchFromGitHub {
              owner = "mattjbray";
              repo = "servant-elm";
              rev = "41793b2d8eddfb2dc35e174457702a43f1486bb0";
              sha256 = "13qk6czck9rlby704fryp8mdryf13lllpkj1arj8bkmhbi5q41gd";
            };
            libraryHaskellDepends = with self; [
              base elm-export lens servant servant-foreign text interpolate mockery
            ];
            doCheck = false;
          };

          opaleye-gen = self.callPackage (
            haskellPackageGen { doFilter = false; } (
              pkgs.fetchFromGitHub{
                owner = "folsen";
                repo = "opaleye-gen";
                rev = "35e50cde7fbab9a2e082e00c4aa09b1dd99b5c43";
                sha256 = "1bi27wfq0zx8s3iyz049lxvnr1fbjzp6ygrdy5lqwrz58xjmk1m5";
              }
            )
          ) {};

          servant-pandoc = overrideAttrs super.servant-pandoc {
            jailbreak = true;
          };

          #
          # New versions for servant-auth
          #
          jose = super.jose_0_5_0_0;

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
          opaleye = overrideAttrs super.opaleye {
            src = pkgs.fetchFromGitHub {
              owner = "tomjaguarpaw";
              repo = "haskell-opaleye";
              rev = "91ac7d0001484f8ef14d3a4dd6e15d9929f43196";
              sha256 = "09vsyv4gk3l43nrisa2rzx2y5w3wh09ln38r3y1dkmnkjmp4jynw";
            };
          };
          countable-inflections = overrideAttrs super.countable-inflections {
            src = pkgs.fetchFromGitHub {
              owner = "folsen";
              repo = "countable-inflections";
              rev = "cb2f8285d3756e4a31d6c8130f07d265f706e23b";
              sha256 = "10s2nmyab3d0kdb12xmz904271lmcr25vn5h845hgqf6qy77bqhk";
            };
            libraryHaskellDepends = with self; [
              base bytestring exceptions pcre-light text pcre-utils regex-pcre-builtin
            ];
          };
          cases = overrideAttrs super.cases {
            jailbreak = true;
          };

          #
          # new versions for servant-elm
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
        };
      };

 haskellPackageGen = { doHaddock ? true, doFilter ? true }: src:
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

  extraEnvPackages = with haskellPackages; [opaleye-gen];

  envWithExtras = pkgs.lib.overrideDerivation drv.env (attrs: {
    buildInputs = attrs.buildInputs ++ extraEnvPackages;
  });

in drv // { env = envWithExtras; }
