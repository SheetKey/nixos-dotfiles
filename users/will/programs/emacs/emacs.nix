{ pkgs, ... }:

{
  home.file.".emacs.d/init.el".source = ./init.el;
  home.file.".emacs.d/config.org".source = ./config.org;
}
