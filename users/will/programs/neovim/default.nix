# neovim default.nix

{ pkgs, config, ... }:

{
  programs.neovim = {
    enable = true;

    viAlias = true;
    vimAlias = true;

    plugins = with pkgs.vimPlugins; [
      # File tree
      nvim-web-devicons
      nvim-tree-lua
      # Colorscheme
      NeoSolarized
      #Indent backline (NOT WORKING AS OF NOW)
      #indent-blankline-nvim
    ];

    extraConfig = ''
      luafile ./lua/settings.lua
    '';
  };

  xdg.configFile."neovim/settings.lua".source = ./lua/setting.lua;
}
