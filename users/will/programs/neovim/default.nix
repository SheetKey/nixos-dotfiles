# neovim default.nix

{ pkgs, config, ... }:

{
  programs.neovim = {
    enable = true;

    viAlias = true;
    vimAlias = true;

    plugins = with pkgs.vimPlugins; [
      NeoSolarized
      indent-backline-nvim
    ];

    extraConfig = ''
      luafile ./lua/settings.lua
    '';
  };
}
