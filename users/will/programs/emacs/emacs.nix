{ pkgs, ... }:

{
  
  home.file.".config/emacs/init.el".source = ./init.el;
  home.file.".config/emacs/config.org".source = ./config.org;

}
