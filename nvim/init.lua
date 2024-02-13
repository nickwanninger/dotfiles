vim.loader.enable()

-- Execute the Vimscript code in the .vim file
vim.cmd('source ' .. vim.fn.stdpath('config') .. '/config.vim')

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
vim.opt.rtp:prepend(lazypath)
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end


require('lazy').setup {
  { import = 'plugins' },

  'folke/zen-mode.nvim',
  'nvim-lua/plenary.nvim',
  'MunifTanjim/nui.nvim',
  'sindrets/diffview.nvim',
  'mbbill/undotree',
  'christoomey/vim-tmux-navigator',
  'RRethy/nvim-base16',
  'preservim/tagbar',

  'lluchs/vim-wren', -- wren (cause it's cool)
  'Shirk/vim-gas',   -- Gnu Assembler
  'dag/vim-fish',    -- Fish Shell
  -- 'kyazdani42/nvim-tree.lua',

  'tpope/vim-fugitive',

  'rhysd/vim-clang-format',
  { "nvim-tree/nvim-web-devicons", lazy = true },
  'weilbith/nvim-code-action-menu',
  'gpanders/nvim-parinfer',
  'bakpakin/fennel.vim',
  'rktjmp/fwatch.nvim',
  { 'echasnovski/mini.nvim', branch = 'stable' },
}

-- Now, configure the rest of the dotfiles!
local config = require('core.config')
config.setup {}
