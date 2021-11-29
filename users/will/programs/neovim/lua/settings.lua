-- Neovim setting.lua
local opt = vim.opt
local g = vim.g

-- Lazy load files
-- dofile("./nvim-tree.lua")

vim.cmd [[
  " NeoSolarized Colorscheme settings
  colorscheme NeoSolarized
  set background=dark

  nnoremap <C-p> :NvimTreeToggle<CR>
]]

-- Enable plugins
require('colorizer').setup()
require('pears').setup()
require'nvim-treesitter.configs'.setup{
    # ensure_installed = "all",
    highlight = {
        enable = true
    },
}


-- local map = vim.api.nvim_set_keymap
-- options = { noremap = true }
-- map('n', '<C-p>', 'NvimTreeToggle <CR>', options)

-- Indent line
-- g.indent_blankline_char = '⸽'

-- Perfromance
opt.shell = "zsh"

-- Colors
-- opt.termguicolors = true

-- Indentation
opt.smartindent = true
opt.tabstop = 4
opt.shiftwidth = 4
opt.shiftround = true
opt.expandtab = true
opt.scrolloff = 3

-- Set clipboard to use system clipboard
opt.clipboard = "unnamedplus"

-- Use mouse
opt.mouse = "a"

-- UI settings
opt.cursorline = true
opt.relativenumber = true
opt.number = true

-- Miscellaneous
opt.ignorecase = true
opt.ttimeoutlen = 5
opt.compatible = false
