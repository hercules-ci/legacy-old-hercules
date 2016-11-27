{ backend }:

with import (fetchTarball https://github.com/NixOS/nixpkgs/archive/e725c927d4a09ee116fe18f2f0718364678a321f.tar.gz) {};

stdenv.mkDerivation {
 name = "hydra-frontend";

 src = ./.;

 buildInputs = [ elmPackages.elm nodejs ];

 patchPhase = ''
   patchShebangs node_modules/webpack
 '';

 buildPhase = ''
   ${backend}/bin/gen-elm src
  
   # https://github.com/NixHercules/hercules/issues/3
   sed -i "s@'@@g" src/Hercules.elm

   npm run build
 '';

 installPhase = ''
   mkdir $out
   cp dist/* $out/
 '';
}
