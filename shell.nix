{
  nixpkgs ? import <nixpkgs> { },
  ghcVersion ? "9103",
}:

let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${"ghc" + ghcVersion}.ghcWithPackages (ps: with ps; [
    heaps
    QuickCheck
  ]);
in

pkgs.stdenv.mkDerivation {
  name = "haskell-tajm";
  buildInputs = (with pkgs; [
	(haskell-language-server.override { supportedGhcVersions = [ ghcVersion ]; })
	hlint
  ]) ++ [
  	ghc
  ];
  # shellHook = "eval $(egrep ^export ${ghc}/bin/ghc";
}
