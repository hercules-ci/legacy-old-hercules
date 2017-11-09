{ backend ? (import ./../backend {})
, pkgs ? (import ./../pkgs.nix) {}
, backendURL ? "http://localhost:3000/api" }:

with pkgs;

stdenv.mkDerivation {
 name = "hercules-frontend";

 src = ./.;

 buildInputs = [ elmPackages.elm elmPackages.elm-format nodejs ];

 patchPhase = ''
   patchShebangs node_modules/webpack
 '';

 # https://github.com/NixHercules/hercules/issues/3
 buildHercules = "${backend}/bin/gen-elm src && sed -i \"s@'@@g\" src/Hercules.elm";

 BACKEND_URL = backendURL;

 buildPhase = ''
   npm run build
 '';

 installPhase = ''
   mkdir $out
   cp -R dist/* $out/
 '';
}
