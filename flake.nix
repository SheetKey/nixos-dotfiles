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
    
      laptop = lib.nixosSystem {
	inherit system;

        modules = [
          ./configuration.nix ./hosts/laptop.nix

	  home-manager.nixosModules.home-manager {

            home-manager.useGlobalPkgs = true;
	    home-manager.useUserPackages = true;
            home-manager.users.will = import ./users/will/home.nix;


	  }
	];
      };

      vmtest = lib.nixosSystem {
        inherit system;

	modules = [
          ./configuration.nix ./hosts/vmtest.nix

	  home-manager.nixosModules.home-manager {
            
            home-manager.useGlobalPkgs = true;
	    home-manager.useUserPackages = true;
            home-manager.users.will = import ./users/will/home.nix;

	  }
	];
      };

      nixos2vmbox = lib.nixosSystem {
        inherit system;

	modules = [
	  ./configuration.nix ./hosts/nixos2vbox.nix

	  home-manager.nixosModules.home-manager {
            
            home-manager.useGlobalPkgs = true;
	    home-manager.useUserPackages = true;
            home-manager.users.will = import ./users/will/home.nix;

	  }
	];
      };

    };

  };
}
