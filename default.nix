{ pkgs ? (import ./pkgs.nix) {}}:

rec {
  backend = import ./backend/default.nix { inherit pkgs; };
  frontend = import ./frontend/default.nix { inherit pkgs backend; };
}
