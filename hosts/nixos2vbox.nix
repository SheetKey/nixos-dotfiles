{ config, lib, pkgs, modulesPath, ... }:

{

  networking.hostName = "vbox";

  environment.systemPackages = with pkgs; [

  ];

  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  # MANDATORY BOOT OPTIONS

  boot.loader.systemd-boot EFI boot loader;
  boot.loader.efi.canTouchEfiVariables = true;

  # COPIED FROM GENERATED "hardward-configuration.nix"

  boot.initrd.availableKernelModules = [ "ata_piix" "ohci_pci" "ahci" "sd_mod" "sr_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-label/nixos";
      fsType = "ext4";
    };
  
  fileSystems."/boot" = 
    { device = "/dev/disk/by-label/boot";
      fsType = "vfat";
    };

  swapDevices = 
    [ { device = "/dev/disk/by-label/swap"; }
    ];

  virtualisation.virtualbox.guest.enable = true;

}
