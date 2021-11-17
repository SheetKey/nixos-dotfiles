# Polybar default.nix

{ config, pkgs, ... }:

let
  bars = builtins.readFile ./bars.ini;
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
    enable = false;

    config = ./config.ini;

    extraConfig = bars + customMods;

    script = ''
      polybar top &
    '';
  };
}
