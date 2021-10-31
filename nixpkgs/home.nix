{ config, lib, pkgs, stdenv, ... }:


{
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "will";
  home.homeDirectory = "/home/will";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.11";



  imports = 
    [
      ./programs/xmonad/default.nix
      ./programs/zsh/default.nix
      ./programs/rofi/default.nix
      ./programs/xmobar/default.nix

      ./services/picom/default.nix
    ];
}
