{ config, pkgs, lib, ... }:

{
  imports =
    [
      ./scripts/default.nix
    ];

  # Nix preferences: unstable version
  nix = {
    settings.auto-optimise-store = true;
    #Garbage collection
    gc = {
      automatic = true;
      dates = "daily";
      options = "--delete-older-than 30d";
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

  # for iphone
  services.usbmuxd.enable = true;

  # Enable the X11 windowing system.
  services.xserver = {
  	enable = true;
    # Keyboard US and Polytonic Greek
    xkb = {
	    layout = "us,gr";
      variant = ",polytonic";
      options = "grp:shifts_toggle, ctrl:nocaps";
    };

	  displayManager = {
      lightdm.enable = true;
	    defaultSession = "none+xmonad";
      autoLogin = {
        enable = false;
        user = "will";
      };
    };

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = builtins.readFile ./users/will/programs/xmonad/xmonad.hs;
    };

    windowManager.awesome = {
      enable = true;
    };

	  libinput = {
      enable = true;
	    touchpad.naturalScrolling = false;
	  };
  };

  services.emacs = {
    enable = false;
    install = true;
    defaultEditor = true;
    package = with pkgs; (emacsWithPackagesFromUsePackage
      {
        config = ./users/will/programs/emacs/config.el;
        package = pkgs.emacs-unstable;
        alwaysEnsure = false;
      }
    );
  };

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;
  services.pipewire.enable = false;

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
  security.wrappers = {
    pmount = {
      source = "${pkgs.pmount}/bin/pmount";
      owner = "root";
      group = "root";
      setuid = true;
    };
    pumount = {
      source = "${pkgs.pmount}/bin/pumount";
      owner = "root";
      group = "root";
      setuid = true;
    };
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    home-manager

    #Terminal emulator
    wget
    git
    neovim

    # xkblayout-state
    xkblayout-state

    # installed globally for xmonad
    dzen2
    feh
  ];

  programs.zsh.enable = true;

  services.logind = {
    extraConfig = "HandlePowerKey=suspend";
    lidSwitch = "suspend";
  };

  programs.slock.enable = true;

  programs.xss-lock = {
    enable = true;
  };

  # Install fonts
  fonts = {
    packages = with pkgs; [
      nerd-fonts.ubuntu
      nerd-fonts.ubuntu-mono
      nerd-fonts.fira-mono
      nerd-fonts.fira-code
      nerd-fonts.mononoki
      font-awesome
      material-icons
      emacs-all-the-icons-fonts
    ];
    fontDir.enable = true;
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  services.syncthing = {
    enable = true;
    user = "will";
    dataDir = "/home/will/Documents";
    configDir = "/home/will/.config/syncthing";
    openDefaultPorts = true;
  };

  programs.kdeconnect.enable = true;

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = false;

  # ADD stateVersion TO HOST CONFIG BASED ON THE GENERATER "configuration.nix"

  #system.stateVersion = "21.05"; # Did you read the comment?

}

