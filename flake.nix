{
  description = "My system config.";

  inputs = {

    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    protonhax-git = {
      url = "github:jcnils/protonhax";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, home-manager, emacs-overlay, protonhax-git, ... }: 

    let 

      system = "x86_64-linux";

      lib = nixpkgs.lib;

      protonhaxSrc = (builtins.readFile "${protonhax-git}/protonhax");
    in {

      nixosConfigurations = {

        asus1 = lib.nixosSystem {
          inherit system;

	        modules = [

	          (args@{pkgs, ... }: {
              nixpkgs.overlays = [ emacs-overlay.overlay ];
              scripts.protonhax-src = protonhaxSrc;
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

        serval = lib.nixosSystem {
          inherit system;
          
          modules = [
            ({pkgs, ... }: {
              nixpkgs.overlays = [emacs-overlay.overlay ];
              scripts.protonhax-src = protonhaxSrc;
              imports =
                [
                  ./configuration.nix
                  ./hosts/serval.nix
                ];
            })
            
            home-manager.nixosModules.home-manager {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.will = {
                imports = [ ./users/will/home.nix ];
              };
            }
          ];
        };

      };
    };
}
