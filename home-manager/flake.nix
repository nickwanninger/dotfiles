{
  description = "There's no place like home. There's no place like home. There's no place like home. (...)";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/25.05";
    home-manager = {
      url = "github:nix-community/home-manager/release-25.05";  # match Home Manager release to nixpkgs
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };


  outputs = { nixpkgs, home-manager, ... }:
    let
      cheeseMachine = {
        system = "x86_64-linux";
        username = "nick";
        home = "/home/nick";
        graphical = false;
        local = false;
      };
      machines = {
        # Dev machines at work
        "nick@roquefort" = cheeseMachine;
        "nick@dubliner" = cheeseMachine;


        # My Macbook Pro
        "nick@caladan" = {
          system = "aarch64-darwin";
          username = "nick";
          home = "/Users/nick";
          graphical = true;
          local = true;
        };

        # The Linux VM on my Macbook
        "nick@quark" = {
          system = "aarch64-linux";
          username = "nick";
          home = "/home/nick";
          graphical = false;
          local = true;
        };
      };


      homeManagerConfiguration = config:
      let
        pkgs = import nixpkgs {
          system = config.system;
          overlays = [ ];
          config = {
            allowUnfree = true;
          };
        };
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
