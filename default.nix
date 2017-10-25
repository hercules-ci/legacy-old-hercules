{ pkgs ? (import ./pkgs.nix) {}}:

rec {
  backend = import ./backend/default.nix { inherit pkgs; };
  frontend = import ./frontend/default.nix { inherit pkgs backend; };
  docs = import ./docs/default.nix { inherit pkgs backend; };
  storeLib = import ./store { inherit pkgs; };
  haskellNixBuild = import ./haskell-nix-build { inherit pkgs storeLib; };
}
