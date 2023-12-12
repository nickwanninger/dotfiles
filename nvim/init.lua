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



local tangpath = vim.fn.stdpath("data") .. "/tangerine"
vim.opt.rtp:prepend(tangpath)
if not vim.loop.fs_stat(tangpath) then
  vim.fn.system({
    "git", "clone", "--filter=blob:none", "https://github.com/udayvir-singh/tangerine.nvim", tangpath,
  })

  vim.fn.system({
    "git", "-C", tangpath, "checkout", "v2.8",
  })
end



-- local nvim_dir = vim.fn.stdpath [[config]]
-- require('tangerine').setup {
--   -- Start by using the init.fnl file
--   vimrc   = nvim_dir .. "/fnl/init.fnl",
--   -- And set the 'include path' for fennel files to $RT/fnl
--   source  = nvim_dir .. "/fnl",
--   target = vim.fn.stdpath [[data]] .. "/tangerine",
--   compiler = {
--     verbose = false,
--     hooks = { "oninit", "onsave" },
--   }
-- }

require('lazy').setup {
  { import = 'plugins' },
  -- { 'udayvir-singh/tangerine.nvim',
  --   priority = 1000,
  --   config = function()
  --     require('tangerine').setup {
  --      -- Start by using the init.fnl file
  --      vimrc   = vim.fn.stdpath [[config]] .. "/fnl/init.fnl",
  --      -- And set the 'include path' for fennel files to $RT/fnl
  --      source  = vim.fn.stdpath [[config]] .. "/fnl",
  --      target = vim.fn.stdpath [[data]] .. "/tangerine",
  --      compiler = {
  --        verbose = false,
  --        hooks = { "oninit", "onsave" },
  --      }
  --     }
  --   end
  -- },

  'folke/zen-mode.nvim',
  'nvim-lua/plenary.nvim',
  'MunifTanjim/nui.nvim',
  'nvim-telescope/telescope.nvim',
  'sindrets/diffview.nvim',
  'mbbill/undotree',
  'christoomey/vim-tmux-navigator',
  'RRethy/nvim-base16',
  'preservim/tagbar',
  
  'lluchs/vim-wren', -- wren (cause it's cool)
  'Shirk/vim-gas', -- Gnu Assembler
  'dag/vim-fish', -- Fish Shell
  -- 'kyazdani42/nvim-tree.lua',
  


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