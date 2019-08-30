let
  pkgs = (import ./nix/pkgs.nix).pkgs;
in
pkgs.mkShell {
  buildInputs = import ./nix/inputs.nix;
}