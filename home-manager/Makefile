userAtHost:=${USER}@$(shell hostname)

switch:
	nix shell nixpkgs#home-manager --command home-manager switch --flake .#${userAtHost}
