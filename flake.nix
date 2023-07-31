{
  description = "My system config.";

  inputs = {

    nixpkgs.url = "github:nixos/nixpkgs/master";

    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nixpkgs, home-manager, emacs-overlay, ... }: 

  let 

    system = "x86_64-linux";

    lib = nixpkgs.lib;

  in {

    nixosConfigurations = {

      asus1 = lib.nixosSystem {
        inherit system;

	      modules = [

	        ({pkgs, ... }: {
            nixpkgs.overlays = [ emacs-overlay.overlay ];
            imports =
              [
	              ./configuration.nix
                ./hosts/asus1.nix
              ];
	        })

	        home-manager.nixosModules.home-manager {
	          home-manager.useGlobalPkgs = true;
	          home-manager.useUserPackages = true;
            home-manager.users.will = {
              imports = [
                ./users/will/home.nix 
              ];
            };
	        }
	      ];
      };

    };
  };
}
