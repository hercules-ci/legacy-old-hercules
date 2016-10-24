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
              owner = "mattjbray";
              repo = "elm-export";
              rev = "3dfafc7a717003ff4374119ff6f60e5b56868d8f";
              sha256 = "009qw5a0pg095yj8h19r036xh622wvih83nxhs3jgmlbvb4a4dlr";
            };
          };
          # Still not quite working
          servant-elm = overrideAttrs super.servant-elm {
            jailbreak = true;
          };
        };
      };

  f = import ./default.nix;

  drv = haskellPackages.callPackage f {};

in

  drv.env
