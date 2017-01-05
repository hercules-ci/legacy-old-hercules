{ pkgs ? (import ./../pkgs.nix) {}
, backend ? (import ./../backend {})
}:

with pkgs;

stdenv.mkDerivation {
  name = "hercules-docs";

  src = ./.;

  buildInputs = [ python3Packages.sphinx ];

  # TODO: generate API from backend

  buildFlags = ["html"];


  installPhase = ''
    mkdir $out
    cp -R _build/html $out
  '';
}
