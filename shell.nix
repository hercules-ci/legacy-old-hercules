{ pkgs ? import <nixpkgs> {} }:

let
  haskellPackages = pkgs.haskell.packages.ghc801.override{
    overrides =
      let overrideAttrs = package: newAttrs: package.override (args: args // {
              mkDerivation = expr: args.mkDerivation (expr // newAttrs);
            });
      in self: super: {
          elm-export = overrideAttrs super.elm-export {
            src = pkgs.fetchFromGitHub {
              owner = "expipiplus1";
              repo = "elm-export";
              rev = "5c6b39f3a0ace445c85c939822d6f10007ae8f11";
              sha256 = "0mjq2y51jb26zxc8jqfs2cyvnczj54ic1zaad45gqh84vnx2jq5p";
            };
          };
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
        };
      };

  f = import ./default.nix;

  drv = haskellPackages.callPackage f {};

in

  drv.env
