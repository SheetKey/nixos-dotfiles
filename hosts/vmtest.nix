{ config, lib, pkgs, modulesPath, ... }:

{

  networking.hostName = "laptop";

  environment.systemPackages = with pkgs; [

  ];

  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  
  boot.initrd.availableKernelModules = [ "ata_piix" "ohci_pci" "ahci" "sd_mod" "sr_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = 
    { device = "dev/disk/by-uuid/924565f0-09ac-4584-8763-9732f043cceb";
      fsType = "ext4";
    };

  swapDevices = 
    [ { device = "/dec/disk/by-uuid/3c8ef464-a4b6-4910-b6da-a2d89552da1c";}
    ];

  virtualisation.virtualbox.guest.enable = true;

}
