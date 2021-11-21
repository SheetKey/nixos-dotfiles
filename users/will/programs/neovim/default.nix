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
      luafile home/will/dotfiles/nixos-dotfiles/users/will/programs/neovim/lua/settings.lua
    '';
  };
}
