{ pkgs, ... }:

{

  home.file.".config/awesome/rc.lua".source = ./rc.lua;

  home.file.".config/awesome/mytheme.lua".source = ./mytheme.lua;

  home.file.".config/awesome/default".source = ./default;
}
