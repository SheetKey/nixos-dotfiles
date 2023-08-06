{ config, lib, pkgs, modulesPath, ... }:

{
  networking = {
    hostName = "servalNixos";
    networkmanager.enable = true;
  };
  
  environment.systemPackages = with pkgs; [
    pciutils
    usbutils
    lshw
    system76-firmware
    firmware-manager
    system76-keyboard-configurator
  ];

  hardware.system76.enableAll = true;

  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.nvidia = {
    modesetting.enable = true;
    prime = {
      offload.enable = true;
      intelBusId = "PCI:0:2:0";
      nvidiaBusId = "PCI:1:0:0";
    };
  };

  programs.steam.enable = true;

  ###############################################

  # THE BELOW CODE IS COPIED FROM THE AUTO-GENERATE 'configuration.nix'.
  # IT MAY CONTAIN EDITS.
  
  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?

  ###############################################

  # THE BELOW CODE IS COPIED FROM THE AUTO-GENERATED 'hardware-configuration.nix'.
  # IT MAY CONTAIN EDITS.

  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];
  
  boot.initrd.availableKernelModules = [ "xhci_pci" "thunderbolt" "nvme" "usb_storage" "sd_mod" "sdhci_pci" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];
  # boot.extraModulePackages = with config.boot.kernelPackages; [ system76 system76-power system76-scheduler system76-io system76-acpi ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/6c42a9d0-1a31-4cf5-a478-f9794c8ac19c";
      fsType = "btrfs";
      options = [ "subvol=root" "compress=zstd" ];
    };

  fileSystems."/home" =
    { device = "/dev/disk/by-uuid/6c42a9d0-1a31-4cf5-a478-f9794c8ac19c";
      fsType = "btrfs";
      options = [ "subvol=home" "compress=zstd" ];
    };

  fileSystems."/nix" =
    { device = "/dev/disk/by-uuid/6c42a9d0-1a31-4cf5-a478-f9794c8ac19c";
      fsType = "btrfs";
      options = [ "subvol=nix" "compress=zstd" "noatime" ];
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/39A3-8B82";
      fsType = "vfat";
    };

  swapDevices = [ ];

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.eno0.useDHCP = lib.mkDefault true;
  # networking.interfaces.wlp0s20f3.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
