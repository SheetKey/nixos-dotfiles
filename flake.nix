{
  description = "My system config.";

  inputs = {

    nixpkgs.url = "nixpkgs/nixos-unstable";

    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

  };

  outputs = inputs@{ self, nixpkgs, home-manager, ... }: 

  let 

    system = "x86_64-linux";

    pkgs = import nixpkgs {
      
      inherit system;

      config = {
	allowUnfree = true;
      };

    };

    lib = nixpkgs.lib;

  in {

    nixosConfigurations = {
      ### Nixos hostname
      nixos = lib.nixosSystem {
	inherit system;

        modules = [
          ./configuration.nix
	];
      };

    };

  };
}
