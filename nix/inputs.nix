let
  pkgs = (import ./pkgs.nix).pkgs;

  stdenv = pkgs.stdenv;

  shakePkgs = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [ghc shake]);
in
[
  shakePkgs
]