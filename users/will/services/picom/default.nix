{ lib, pkgs, ... }:

{
  home.file = {
    ".config/picom/picom.conf".source = ./picom.conf;
  };
}
