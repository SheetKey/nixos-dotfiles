# Polybar default.nix

{ config, pkgs, ... }:

let
  xmonad = ''
    [module/xmonad]
    type = custon/script
    exec = ${pkgs.xmonad-log}/bin/xmonad-log

    tail = true
  '';

  customMods = xmonad;

in
{
  services.polybar = {
    enable = true;

    config = ./config.ini;

    extraConfig = customMods;

    script = ''
      polybar bar &
    '';
  };
}
