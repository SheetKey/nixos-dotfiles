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
    bash
    # DO NOT ADD NEOVIM TO "home.packages": LEAVE IN "configuration.nix"
    # neovim
    neofetch
    htop
    #xorg.kill
    lf
    nix-prefetch-github
    ripgrep
    unzip
    hardinfo
    pmount

    # midi
    timidity
    # tab editor
    tuxguitar

    # text to speech
    poppler_utils
    espeak-classic
    vlc
    # piper-tts #CURRENTLY BROKEN; TRY AGAIN WHEN NEEDED (onnx dependency doesn't build)

    lilypond

    # Latex
    texlive.combined.scheme-full
    auctex
    tikzit

    # Viewer
    zathura
    nomacs

    # Hakell
    ### haskell-language-server

    # Matlab (Octave of course)
    (octaveFull.withPackages (opkgs: with opkgs; [ symbolic ]))

    # GUI stuff
    brave
    firefox
    # nyxt #broken sbcl dep
    trayer
    networkmanagerapplet
    cbatticon
    pa_applet
    #mathematica
    zoom-us
    pavucontrol
    brightnessctl
    upower
    scrot #screenshots
    spectacle
    libreoffice
    krita
    goxel
    zotero

    glxinfo

    # Games
    boohu
    prismlauncher
    minecraft
    #mindustry
    #freeciv_qt
    #widelands
    #unnethack
    mgba

    # window manager related
    nitrogen
    picom
    rofi

    # Stuff for emacs
    ncurses
    sqlite

    aspell
    aspellDicts.en
    
      # Stuff for vterm
    cmake
    libtool
    libvterm
  ];



  imports = 
    [
      ./programs/emacs/default.nix
      ./programs/xmonad/default.nix
      ./programs/zsh/default.nix
      ./programs/bash/default.nix
      ./programs/rofi/default.nix
      ./programs/xmobar/default.nix
      ./programs/lf/default.nix
      ./programs/alacritty/default.nix
      ./programs/neovim/default.nix
      ./programs/direnv/default.nix

      ./programs/nyxt/default.nix

      ./programs/awesomewm/default.nix

      ./services/picom/default.nix
      ./services/polybar/default.nix
      ./services/kdeconnect/default.nix

      ./xdg/userDirs/default.nix
    ];
}
