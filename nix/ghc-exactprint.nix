{ mkDerivation, base, bytestring, containers, Diff, directory
, filemanip, filepath, free, ghc, ghc-boot, ghc-paths, HUnit, mtl
, silently, stdenv, syb
}:
mkDerivation {
  pname = "ghc-exactprint";
  version = "0.5.8.2";
  sha256 = "961dde178df96c123d12a362f64f7ef43228176fd3a1b876a8fecc75df8694a3";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers directory filepath free ghc ghc-boot
    ghc-paths mtl syb
  ];
  testHaskellDepends = [
    base bytestring containers Diff directory filemanip filepath ghc
    ghc-boot ghc-paths HUnit mtl silently syb
  ];
  doCheck = false;
  description = "ExactPrint for GHC";
  license = stdenv.lib.licenses.bsd3;
}
