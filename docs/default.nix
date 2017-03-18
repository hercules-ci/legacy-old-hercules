{ pkgs ? (import ./../pkgs.nix) {}
, backend ? (import ./../backend {})
}:

with pkgs;


stdenv.mkDerivation {
  name = "hercules-docs";

  src = ./.;

  propagatedBuildInputs = with python3Packages; [ sphinxcontrib-openapi sphinx_rtd_theme ];

  preBuild = ''
    ${backend}/bin/gen-docs
    echo "HTTP API" > api.rst
    echo "********" >> api.rst
    echo ".. openapi:: api.yml" >> api.rst
  '';

  buildFlags = ["html"];

  installPhase = ''
    mkdir $out
    cp -R _build/html/* $out
  '';
}
