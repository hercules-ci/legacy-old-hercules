{ pkgs ? import <nixpkgs> {} }:

let
  haskellPackages = pkgs.haskell.packages.ghc801.override{
    overrides =
      let overrideAttrs = package: newAttrs: package.override (args: args // {
              mkDerivation = expr: args.mkDerivation (expr // newAttrs);
            });
      in self: super: {
          servant-elm = overrideAttrs super.servant-elm {
            src = pkgs.fetchFromGitHub {
              owner = "mattjbray";
              repo = "servant-elm";
              rev = "342646d6f1ea9adf8886322f26abca5adfd8a48b";
              sha256 = "0jls7kfrq95i6chl8ljaqjyxmsg6f5qabvj580r3cvx79bhpr7r0";
            };
            libraryHaskellDepends = with self; [
              base elm-export lens servant servant-foreign text interpolate mockery
            ];
            doCheck = false;
          };

          opaleye-gen = with self;
            mkDerivation {
              pname = "opaleye-gen";
              version = "0.1.0.0";
              src = pkgs.fetchFromGitHub{
                owner = "folsen";
                repo = "opaleye-gen";
                rev = "35e50cde7fbab9a2e082e00c4aa09b1dd99b5c43";
                sha256 = "1bi27wfq0zx8s3iyz049lxvnr1fbjzp6ygrdy5lqwrz58xjmk1m5";
              };
              isLibrary = false;
              isExecutable = true;
              executableHaskellDepends = [
                base bytestring cases containers countable-inflections opaleye
                optparse-applicative postgresql-simple product-profunctors text
                time
              ];
              homepage = "https://github.com/folsen/opaleye-gen#readme";
              description = "A lightweight program to generate Opaleye boilerplate from a database";
              license = stdenv.lib.licenses.bsd3;
            };

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
            src = pkgs.fetchFromGitHub {
              owner = "expipiplus1";
              repo = "elm-export";
              rev = "5c6b39f3a0ace445c85c939822d6f10007ae8f11";
              sha256 = "0mjq2y51jb26zxc8jqfs2cyvnczj54ic1zaad45gqh84vnx2jq5p";
            };
          };
        };
      };

 haskellPackageGen = { doHaddock ? true }: src:
    let filteredSrc = pkgs.lib.cleanSource src;
        package = pkgs.runCommand "default.nix" {} ''
          ${pkgs.haskell.packages.ghc801.cabal2nix}/bin/cabal2nix \
            ${filteredSrc} \
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
