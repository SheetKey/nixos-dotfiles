{ config, pkgs, ... }:

{
  imports =
    [
      ./scripts/default.nix
    ];


  # Nix preferences: unstable version
  nix = {
    # Taken care of by flake
    # package = pkgs.nixUnstable;

    #Garbage collection
    gc = {
      automatic = true;
      dates = "daily";
    };

    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };
  nixpkgs.config.allowUnfree = true;

############## ADD BOOT SETTINGS IN HOST SPECIFIC CONFIG#########################
  # Use the GRUB 2 boot loader.
  # boot.loader.grub.enable = true;
  # boot.loader.grub.version = 2;
  # boot.loader.grub.efiSupport = true;
  # boot.loader.grub.efiInstallAsRemovable = true;
  # boot.loader.efi.efiSysMountPoint = "/boot/efi";
  # Define on which hard drive you want to install Grub.
  # boot.loader.grub.device = "/dev/sda"; # or "nodev" for efi only
#################################################################################

  # Set up locales.
  time.timeZone = "America/New_York";
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "FiraCode";
    keyMap = "us";
  };

##################### ADD NETWORK SETTINGS TO HOST SPECIFIC CONGIF ########################
  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  # networking.useDHCP = false;
  # networking.interfaces.enp0s3.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";


  # Enable the X11 windowing system.
  services.xserver = {
  	enable = true;
    # Keyboard US and Polytonic Greek
	  layout = "us,gr";
    xkbVariant = ",polytonic";
    xkbOptions = "grp:shifts_toggle, caps:escape";


	  displayManager.lightdm.enable = true;
	  displayManager.defaultSession = "none+xmonad";
	  windowManager.xmonad = {
		  enable = true;
		  enableContribAndExtras = true;
		  extraPackages = hpkgs: [
			  hpkgs.xmonad
			  hpkgs.xmonad-contrib
			  hpkgs.xmonad-extras
			];
	  };
	  libinput = {
      enable = true;
	    touchpad.naturalScrolling = false;
	  };
  };

  services.emacs = {
    enable = true;
    package = pkgs.emacsGitNativeComp;
  };

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.will = {
    isNormalUser = true;
    home = "/home/will";
    extraGroups = [ "wheel" "networkmanager" "audio" ]; 
    shell = pkgs.zsh;

    initialPassword = "12";

  };

  #no sudo passwd
  security.sudo = {
  enable = true;
  wheelNeedsPassword = true;
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    home-manager

    #Terminal emulator
    wget
    git
    neovim

    #Window manager stuff
    xmobar
    polybar

    # xkblayout-state
    xkblayout-state
  ];

  # Install fonts
  fonts.fonts = with pkgs; [
    nerdfonts
    font-awesome
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # ADD stateVersion TO HOST CONFIG BASED ON THE GENERATER "configuration.nix"

  #system.stateVersion = "21.05"; # Did you read the comment?

}

