# xmonad default.nix


{
  xsession = {
    enable = false;

    windowManager.xmonad = {
      enable = false;
      enableContribAndExtras = true;
      extraPackages = hp: [
        hp.monad-logger
	      hp.xmonad-contrib
	      hp.xmonad-extras
	      hp.dbus
      ];
      config = ./config.hs;
    };
  };
}
