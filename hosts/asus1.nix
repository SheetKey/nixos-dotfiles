{ config, lib, pkgs, modulesPath, ... }:

{

  networking.hostName = "Nixos";
  # networking.networkmanager.enable = true;
  networking.wireless.enable = true;
  networking.wireless.userControlled.enable = true;
  # interface for wifi usb card
  networking.interfaces.wlp0s20f0u1.useDHCP = true;

  environment.systemPackages = with pkgs; [
    pciutils
    usbutils
    lshw
  ];

  hardware.enableRedistributableFirmware = true;

  # COPIED FROM GENERATED configuration.nix
    system.stateVersion = "21.11";

    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;

  # COPIED FROM GENERATED hardware-configuration.nix

    imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

    boot.initrd.availableKernelModules = [ "xhci_pci" "nvme" "usb_storage" "sd_mod" "alcor" ];
    boot.initrd.kernelModules = [ ];
    boot.kernelModules = [ "kvm-intel" "iwlwifi" ];
    boot.extraModulePackages = [ ];

    fileSystems."/" =
      { device = "/dev/disk/by-label/NIXOS";
        fsType = "ext4";
      };

    fileSystems."/boot" =
      { device = "/dev/disk/by-label/BOOT";
        fsType = "vfat";
      };

    swapDevices =
      [ { device = "/dev/disk/by-label/SWAP"; }
      ];

    powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
    hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

}
