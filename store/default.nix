{ pkgs ? import <nixpkgs> {} }:

let p = { stdenv, fetchurl, cmake, nixUnstable, boost }:
        stdenv.mkDerivation rec {
          name = "dummy-store";
        
          src = builtins.filterSource 
            (path: type: 
              baseNameOf path != "result" && 
              baseNameOf path != "build") 
            ./.;

          buildInputs = [ cmake nixUnstable boost ];
        };
in pkgs.callPackage p {}
