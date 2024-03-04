{ lib, pkgs, ... }:

{
  home.file = {
    ".config/zathura/zathurarc".source = ./zathurarc;
  };
}
