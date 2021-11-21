# neovim default.nix

{ pkgs, config, ... }:

{
  programs.neovim = {
    enable = true;

    viAlias = true;
    vimAlias = true;

    plugins = with pkgs.vimPlugins; [
      NeoSolarized
      #indent-blankline-nvim
    ];

    extraConfig = ''
      luafile ./lua/settings.lua
    '';
  };
}
