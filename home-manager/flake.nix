{
  description = "There's no place like home. There's no place like home. There's no place like home. (...)";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/25.05";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

  };


  outputs = { nixpkgs, home-manager, ... }:
    let
      machines = {
        # Dev machine at work
        "nick@roquefort" = {
          system = "x86_64-linux";
          username = "nick";
          home = "/home/nick";
          graphical = false;
          local = false;
          stateVersion = "23.05";
        };


        # My Macbook Pro
        "nick@caladan" = {
          system = "aarch64-darwin";
          username = "nick";
          home = "/Users/nick";
          graphical = true;
          local = true;
          stateVersion = "23.05";
        };

        # The Linux VM on my Macbook
        "nick@quark" = {
          system = "aarch64-linux";
          username = "nick";
          home = "/home/nick";
          graphical = false;
          local = true;
          stateVersion = "23.05";
        };
      };


      homeManagerConfiguration = config:
      let
        pkgs = import nixpkgs { system = config.system; };
      in home-manager.lib.homeManagerConfiguration {
        inherit pkgs;

        modules = [
          {nixpkgs.config.allowUnfree = true; }
          (import ./home.nix { inherit pkgs config; })
        ];
      };
    in {
      homeConfigurations = nixpkgs.lib.attrsets.mapAttrs
        (ident: cfg: homeManagerConfiguration cfg)
        machines;
    };
}
