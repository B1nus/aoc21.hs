{
	inputs = {
		nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
	};

	outputs = { nixpkgs, ... }: let 
		inherit (nixpkgs) lib;
		forAllSystems = lib.genAttrs lib.systems.flakeExposed;
	in {
		devShells = forAllSystems (
			system: let
				pkgs = nixpkgs.legacyPackages.${system};
				ghc = pkgs.haskell.packages.${"ghc" + ghcVersion}.ghcWithPackages (ps: with ps; [
					heaps
					QuickCheck
				]);
				hls = (haskell-language-server.override { supportedGhcVersions = [ ghcVersion ]; });
				hlint = pkgs.hlint;
			in {
				default = pkgs.mkShell {
					packages = [
						ghc
						hlint
						hls
					];
				};
			}
		);
	};
}
