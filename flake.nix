{
  description = "My system config.";

  inputs = {

    nixpkgs.url = "github:nixos/nixpkgs/master";

    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    neovim-nightly-overlay = {
        url = "github:nix-community/neovim-nightly-overlay";
        inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      #inputs.nixpkgs.follows = "nixpkgs";
    };

    nur = {
      url = "github:nix-community/NUR";
      inputs.nixpkgs.follows = "nixpkgs";
    };

  };

  outputs = inputs@{ self, nixpkgs, home-manager, neovim-nightly-overlay, nur, emacs-overlay, ... }: 

  let 

    system = "x86_64-linux";

    pkgs = import nixpkgs {
      
      inherit system;

      config = {
    	allowUnfree = true;
      };

    };

    lib = nixpkgs.lib;

    nur-no-pkgs = import inputs.nur { 
      pkgs = null; 
      nurpkgs = pkgs;
    };

  in {

    nixosConfigurations = {

      asus1 = lib.nixosSystem {
        inherit system;

	    modules = [
	      ./configuration.nix ./hosts/asus1.nix

	      ({ pkgs, ... }: {
	        nixpkgs.overlays = [ neovim-nightly-overlay.overlay
                               emacs-overlay.overlay
                             ];
	      })

	      home-manager.nixosModules.home-manager {
	        home-manager.useGlobalPkgs = true;
	        home-manager.useUserPackages = true;
            home-manager.users.will = {
              imports = [
                ./users/will/home.nix 
                nur-no-pkgs.repos.rycee.hmModules.emacs-init
              ];
            };
	        nixpkgs.overlays = [
	          nur.overlay #emacs-overlay.overlay
	        ];
	      }
	    ];
      };

      nixos3 = lib.nixosSystem {
        inherit system;

	    modules = [
	      ./configuration.nix ./hosts/nixos3.nix

	      ({ pkgs, ... }: {
	        nixpkgs.overlays = [ neovim-nightly-overlay.overlay ];
	      })

	      home-manager.nixosModules.home-manager {
	        home-manager.useGlobalPkgs = true;
	        home-manager.useUserPackages = true;
            home-manager.users.will = {
              imports = [
                ./users/will/home.nix 
                nur-no-pkgs.repos.rycee.hmModules.emacs-init
              ];
            };
	        nixpkgs.overlays = [
	          nur.overlay emacs-overlay.overlay
	        ];
	      }
	    ];
      };
    
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

          ({ pkgs, ... }: {
            nixpkgs.overlays = [ neovim-nightly-overlay.overlay ];
          })

	     home-manager.nixosModules.home-manager {
            
            home-manager.useGlobalPkgs = true;
	        home-manager.useUserPackages = true;
            home-manager.users.will = import ./users/will/home.nix;
            nixpkgs.overlays = [
              nur.overlay emacs-overlay.overlay
            ];

    	  }
    	];
      };

    };

  };
}
