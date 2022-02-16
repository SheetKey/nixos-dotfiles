{ config, lib, pkgs, modulesPath, ... }:

{

  networking.hostName = "nixos";
  networking.networkmanager.enable = true;
  # networking.wireless.enable = true;
  # networking.wireless.userControlled.enable = true;

  networking.useDHCP = false;
  # networking.interfaces.wlp3s0.useDHCP = true;

  environment.systemPackages = with pkgs; [
    pciutils
    usbutils
    lshw


    (octave.withPackages (opkgs: with opkgs; [ symbolic ]))
  ];

  # hardware.enableRedistributableFirmware = true;

  #################### NVIDIA

    services.xserver.videoDrivers = [ "nvidia" ];
    hardware.nvidia.prime = {
      offload.enable = true;
    
      # Bus ID of the Intel GPU. You can find it using lspci, either under 3D or VGA
      intelBusId = "PCI:0:2:0";
    
      # Bus ID of the NVIDIA GPU. You can find it using lspci, either under 3D or VGA
      nvidiaBusId = "PCI:1:0:0";
    };

  ###########################

  # COPIED FROM GENERATED configuration.nix
    system.stateVersion = "21.11";

    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;

  # COPIED FROM GENERATED hardware-configuration.nix

    imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

    boot.initrd.availableKernelModules = [ "xhci_pci" "nvme" "usb_storage" "sd_mod" "alcor" ];
    boot.initrd.kernelModules = [ ];
    boot.kernelModules = [ "kvm-intel" ];
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

    powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
    hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

}
