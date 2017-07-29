update-nix-def:
	cd nix && cabal2nix .. > default.nix

update-shell-def:
	cabal2nix . --shell > shell.nix
