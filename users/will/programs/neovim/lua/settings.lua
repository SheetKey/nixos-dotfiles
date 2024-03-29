-- Neovim setting.lua
local opt = vim.opt
local g = vim.g

-- Lazy load files
-- dofile("./nvim-tree.lua")

vim.cmd [[
  " NeoSolarized Colorscheme settings
  colorscheme NeoSolarized
  set background=dark

  " Latex Vimtex Settings
  let g:tex_flavor='latex'
  let g:vimtex_view_method='zathura'
  "let g:vimtex_quickfix_mode=0

  " Nvim Tree keybinds
  " nnoremap <C-p> :NvimTreeFocus<CR>
  nnoremap <C-c> :NvimTreeClose<CR>

    function! NvimTreeFocusToggle()
        if (&filetype == "NvimTree")
            return "normal \<C-W>\l"
        else
            return ":NvimTreeFocus"
        endif
    endfunction

  nnoremap <C-p> :execute NvimTreeFocusToggle() <CR>

  " Ultisnips settings
  let g:UltiSnipsExpandTrigger='<tab>'
  let g:UltiSnipsJumpForwardTrigger='<tab>'
  let g:UltiSnipsJumpBackwardTrigger='<s-tab>'

  " Bufferline remaps
    " Navigate through buffers in order
    nnoremap <silent>bm :BufferLineCycleNext<CR>
    nnoremap <silent>bn :BufferLineCyclePrev<CR>
    " Move the current buffer forward or backwards
    nnoremap <silent>]b :BufferLineMoveNext<CR>
    nnoremap <silent>[b :BufferLineMovePrev<CR>
    " Sort buffers by directory
    nnoremap <silent>bd :BufferLineSortByDirectory<CR>
]]

-- Enable plugins
require('bufferline').setup{
    options = {
        offsets = {
            {
                filetype = "NvimTree", 
                text = "File Explorer", 
                highlight = "Directory",
                text_align = "left"
            }
        },
    }
}
require('lualine').setup {
    options = {
        theme = 'solarized_dark',
    },
}
require('colorizer').setup()
require('pears').setup()
--require'nvim-treesitter.configs'.setup{
--    -- ensure_installed = "all",
--    highlight = {
--        enable = true
--        -- enable = false
--    },
--}


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
