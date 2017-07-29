update-nix-def:
	cd nix && cabal2nix .. > default.nix
