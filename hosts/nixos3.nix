{ config, lib, pkgs, modulesPath, ... }:

{
  # COPIED FROM GENERATED "configuration.nix"
  system.stateVersion = "21.11";

  networking.hostName = "vbox";

  environment.systemPackages = with pkgs; [

  ];

  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  # MANDATORY BOOT OPTIONS

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # COPIED FROM GENERATED "hardware-configuration.nix"

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

  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  virtualisation.virtualbox.guest.enable = true;

}
