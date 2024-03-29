{
  description = "There's no place like home. There's no place like home. There's no place like home. (...)";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

  };


  outputs = { nixpkgs, home-manager, ... }:
    let
      machines = {
        "nick@roquefort" = {
          system = "x86_64-linux";
          username = "nick";
          home = "/home/nick";
          local = false;
          stateVersion = "23.05";
        };

        "nick@caladan" = {
          system = "aarch64-darwin";
          username = "nick";
          home = "/Users/nick";
          local = true;
          stateVersion = "23.05";
        };
      };


      homeManagerConfiguration = {
        system,
        username,
        home,
        stateVersion,
        local,
      }:
      let
        pkgs = import nixpkgs { inherit system; };
      in home-manager.lib.homeManagerConfiguration {
        inherit pkgs;

        modules = [
          {nixpkgs.config.allowUnfree = true; }
          (import ./home.nix {
            inherit system pkgs username home stateVersion local;
          })
        ];
      };
    in {
      homeConfigurations = nixpkgs.lib.attrsets.mapAttrs
        (ident: cfg: homeManagerConfiguration cfg)
        machines;
    };
}
