# picom default.nix

{
  services.picom = {
    enable = true;
    activeOpacity = "1.0";
    inactiveOpacity = "0.85";
    backend = "glx";
    fade = true;
    fadeDelta = 5;
    shadow = true;
    shadowOpacity = "0.75";
    extraOptions = ''
      unredir-if-possible = false;
      vsync = true;
    '';
  };
}
