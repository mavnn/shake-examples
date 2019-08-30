let
  pkgs = (import ./pkgs.nix).pkgs;

  stdenv = pkgs.stdenv;

  shakePkgs = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [ghc shake]);

  ormoluSrc = pkgs.fetchFromGitHub {
    owner  = "tweag";
    repo   = "ormolu";
    rev    = "d61b9101ca7e1bd9edbc7767eae69e9c7732a0a8";
    sha256 = "1hkrwm1bsriw6pwvml4wlbpfnsvciipdlp2qy8xmmb1wbx0ssiy5";
  };
  ormolu = import ormoluSrc;
in
[
  shakePkgs
  pkgs.gcc
  ormolu.ormolu
]