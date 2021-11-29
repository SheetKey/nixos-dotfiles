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
      nvim-colorizer-lua

      # Languages
      vim-nix

      # Eyecandy
      (nvim-treesitter.withPlugins (plugins: pkgs.tree-sitter.allGrammars))
      pears-nvim
      bufferline-nvim

      #Indent backline (NOT WORKING AS OF NOW)
      #indent-blankline-nvim
    ];

    extraConfig = ''
      set runtimepath^=${./lua}
      set termguicolors
      luafile ${./lua}/settings.lua

      lua << EOF
      vim.defer_fn(function()
        vim.cmd [[
          luafile ${./lua}/nvim-tree.lua
        ]]
      end, 70)
      print('hello 1')
      EOF
    '';
  };
}
