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
    unzip

    # midi
    timidity
    # tab editor
    tuxguitar

    # Latex
    texlive.combined.scheme-full
    auctex

    # Viewer
    zathura

    # Hakell
    ### haskell-language-server

    # Matlab (Octave of course)
    (octaveFull.withPackages (opkgs: with opkgs; [ symbolic ]))

    # GUI stuff
    brave
    trayer
    networkmanagerapplet
    cbatticon
    pa_applet
    mathematica
    zoom-us
    pavucontrol
    brightnessctl
    scrot #screenshots
    spectacle
    libreoffice

    glxinfo

    # Games
    boohu
    superTux
    mindustry
    freeciv_qt
    widelands
    zeroad
    polymc
    unnethack

    # window manager related
    nitrogen
    picom
    rofi

    # Stuff for emacs
    ncurses

    aspell
    aspellDicts.en
    
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
      ./programs/direnv/default.nix

      ./services/picom/default.nix
      ./services/polybar/default.nix

      ./xdg/userDirs/default.nix
    ];
}
