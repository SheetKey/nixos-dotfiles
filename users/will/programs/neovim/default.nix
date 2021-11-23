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
      set runtimepath^=${./lua}
      luafile ${./lua}/settings.lua

      "luafile ${./lua}/nvim-tree.lua
      "lua << EOF 
      "  dofile("${./lua}/nvim-tree.lua")
      "EO

      lua << EOF
      vim.defer_fn(function()
        vim.cmd [[
          luafile ${./lua}/nvim-tree.lua
        ]]
      end, 70)
      print('hello 1')
      EO

      " lua << EOF
      " dofile("${./lua}/nvim-tree.lua")
      " print('hello')
      " EOF
    '';
  };

  # xdg.configFile."nvim/lua/settings.lua".source = ./lua/settings.lua;
  # xdg.configFile."nvim/lua/nvim-tree.lua".source = ./lua/nvim-tree.lua;
  
  # home.file."settings.lua".source = ./lua/settings.lua;
  # home.file."settings.lua".target = .config/nvim/lua;

  # home.file."nvim-tree.lua".source = ./lua/nvim-tree.lua;
  # home.file."nvim-tree.lua".target = .config/nvim/lua;
}
