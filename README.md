# nixos-dotfiles

## Install on a new system

1. Follow [nixos install guide](nixos.org/manual/nixos/stable/) section 2 up to running 
	`nixos-install`. 

2. Edit the generated `configuration.nix`
	1. Add user `jane` with `will` and add `initialPassword` option
	2. Add packages `neovim` and `git` (others are optional, I usually only install these)

3. Run `nixos-install` and create `root` user password.

4. Reboot and remove boot drive.

5. Check internet connection.

6. Create new ssh-key with `ssh-keygen -t ed25519 -C "your_email@email.com"` and add key to github.

7. `eval `ssh-agent`, `ssh-add KEYNAME`.

8. In home directory run, `mkdir dotfiles`, `cd /dotfiles`, `git clone git@github.com:SheetKey/nixos-dotfiles.git`

9. Create new host file in `/hosts` 

10. Add to new host file
	```
	{ config, lib, pkgs, modulesPath, ... }:

	{
	  networking.howName = "HOSTNAME";

	  environment.systemPackages = with pkgs; [
	  # Add any host specific packages here
	  ];

	  # COPY "system.stateVersion" HERE FROM THE GENERATED "configuration.nix" HERE

	  # COPY BOOT OPTIONS FROM THE GENERATED "configuration.nix" HERE

	  # COPY MOST OF FILE FROM THE GENERATED "hardware-configuration.nix" HERE

	}
	```

11. Edit flake.nix to create new "nixosConfiguration"
	```
	NEWHOSTNAME = lib.nixosSystem {
	  inherit system;

	  modules = [
	    ./configuration.nix ./hosts/NEWHOST.nix
	    ({ pkgs, ... }: {
	      nixpkgs.overlays = [ neovim-nightly-overlay.overlay ]:
	    })

	    home-manager.nixosModules.home-manager {
	      home-manager.useGlobalPkgs = true;
	      home-manager.useUserPackages = true;
	      home-manager.user.will = import ./users/will/home.nix;
	      nixpkgs.overlays = [
	        nur.overlay emacs-overlay.overlay
	      ];
	    }
	  ]
	}
	```

12. Build a certain nixosConfiguration.
	```
	nixos-rebuild switch --flake .#CONFIGURATIONNAME
	```
