# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  home-manager-tar = builtins.fetchTarball "https://github.com/nix-community/home-manager/archive/master.tar.gz";

in 
{
  imports =
    [
      (import "${home-manager-tar}/nixos")
    ];

  home-manager.users.will = import ./home.nix
    

  #Nix preferences: unstable version
  nix = {
    package = pkgs.nixUnstable;
    #Garbage collection
    gc = {
      automatic = true;
      dates = "03:15";
    };
  };
  system.autoUpgrade = {
    enable = true;
    channel = "https://nixos.org/channels/nixos-unstable";
  };

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # boot.loader.grub.efiSupport = true;
  # boot.loader.grub.efiInstallAsRemovable = true;
  # boot.loader.efi.efiSysMountPoint = "/boot/efi";
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda"; # or "nodev" for efi only

  networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  time.timeZone = "America/New_York";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp0s3.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "FiraCode";
    keyMap = "us";
  };

  # Enable the X11 windowing system.
  services.xserver = {
  	enable = true;
	#displayManager.lighdm.enable = true;
	displayManager.defaultSession = "none+xmonad";
	windowManager = {
		xmonad.enable = true;
		xmonad.enableContribAndExtras = true;
		xmonad.extraPackages = hpkgs: [
			hpkgs.xmonad
			hpkgs.xmonad-contrib
			hpkgs.xmonad-extras
			];
	};
  };

  #Shell
  users.defaultUserShell = pkgs.zsh;
  
  # nvim default editor
  programs.neovim.defaultEditor = true;

  # Configure keymap in X11
  services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.will = {
    isNormalUser = true;
    home = "/home/will";
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  };

  #no sudo passwd
  security.sudo = {
  enable = true;
  wheelNeedsPassword = false;
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    home-manager

    #Terminal emulator
    wget
    git

    #Window manager stuff
    xmobar
  ];

  fonts.fonts = with pkgs; [
    ubuntu_font_family
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

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?

}

