{
  nixpkgs ? import <nixpkgs> { },
  compiler ? "ghc984",
}:

let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
    heap
    QuickCheck
  ]);
in

pkgs.stdenv.mkDerivation {
  name = "haskell-tajm";
  buildInputs = [ ghc ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc";
}
