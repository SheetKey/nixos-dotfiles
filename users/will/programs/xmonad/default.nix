# xmonad default.nix


{
  xsession = {
    enable = true;

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
