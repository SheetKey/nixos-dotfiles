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



  home.packages = with pkgs; [
    # Terminal
    alacritty
    zsh
    # DO NOT ADD NEOVIM TO "home.packages": LEAVE IN "configuration.nix"
    # neovim
    neofetch
    htop
    #xorg.kill
    lf
    nix-prefetch-github
    ripgrep

    # Latex
    texlive.combined.scheme-full
    auctex

    # Viewer
    zathura

    # Haskell

    haskell-language-server

    # Matlab (Octave of course)
    octaveFull

    # GUI stuff
    brave
    trayer
    networkmanagerapplet
    cbatticon
    pa_applet
    mathematica
    zoom-us
    noisetorch
    pavucontrol
    scrot #screenshots

    # Games
    tlauncher
    boohu
    superTux
    mindustry
    openttd
    freeciv_qt

    # window manager related
    nitrogen
    picom
    rofi

    # Stuff for emacs
    ncurses
      # Stuff for vterm
    cmake
    libtool
    libvterm
  ];



  imports = 
    [
      ./programs/xmonad/default.nix
      ./programs/zsh/default.nix
      ./programs/rofi/default.nix
      ./programs/xmobar/default.nix
      ./programs/lf/default.nix
      ./programs/alacritty/default.nix
      ./programs/neovim/default.nix
      ./programs/emacs/default.nix

      ./services/picom/default.nix
      ./services/polybar/default.nix

      ./xdg/userDirs/default.nix
    ];
}
