{ config, lib, pkgs, modulesPath, ... }:

{
  networking.hostName = "laptop";

  environment.systemPackages = with pkgs; [
    #add battery saver packages: tlp powertop acpi upower
  ];

  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

}
