{ pkgs ? (import ./../pkgs.nix) {}
, backend ? (import ./../backend {})
}:

with pkgs;

stdenv.mkDerivation {
  name = "hercules-docs";

  src = ./.;

  buildInputs = [ python3Packages.sphinx python3Packages.sphinx_rtd_theme ];

  preBuild = ''
    ${backend}/bin/gen-docs
    sed -i '1 i\HTTP API\n********' api.rst
  '';

  buildFlags = ["html"];


  installPhase = ''
    mkdir $out
    cp -R _build/html/* $out
  '';
}
