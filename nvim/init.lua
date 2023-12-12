-- This file is a relatively simple bootstrapping script for my neovim configuration
-- It's main job is to setup a bunch of sane defaults, make sure packer is installed,
-- the plugins are `used`, and the initial sync is performed.
-- As part of that sync, it will install "tangerine", which allows the rest of the
-- configuration to be made in "fennel", which is a lisp

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

  { 'udayvir-singh/tangerine.nvim',
    priority = 1000,
    config = function()
      local nvim_dir = vim.fn.stdpath [[config]]
      require('tangerine').setup {
       -- Start by using the init.fnl file
       vimrc   = nvim_dir .. "/fnl/init.fnl",
       -- And set the 'include path' for fennel files to $RT/fnl
       source  = nvim_dir .. "/fnl",
       target = vim.fn.stdpath [[data]] .. "/tangerine",
       compiler = {
         verbose = false,
         hooks = { "oninit", "onsave" },
       }
      }
    end
  },

  "folke/which-key.nvim",
  'folke/zen-mode.nvim',
  'NeogitOrg/neogit',
  'nvim-lua/plenary.nvim',
  'MunifTanjim/nui.nvim',
  'nvim-telescope/telescope.nvim',
  'sindrets/diffview.nvim',
  'mbbill/undotree',
  'christoomey/vim-tmux-navigator',
  'RRethy/nvim-base16',
  'brenoprata10/nvim-highlight-colors',
  'preservim/tagbar',
  'numToStr/Comment.nvim',
  'lluchs/vim-wren', -- wren (cause it's cool)
  'Shirk/vim-gas', -- Gnu Assembler
  'dag/vim-fish', -- Fish Shell
  'lewis6991/gitsigns.nvim',
  'kyazdani42/nvim-tree.lua',
  'rcarriga/nvim-notify',
  'voldikss/vim-floaterm',
  'ms-jpq/coq_nvim',
  'ms-jpq/coq.artifacts',
  'neovim/nvim-lspconfig',
  'ray-x/lsp_signature.nvim',
  'rhysd/vim-clang-format',
  'ErichDonGubler/lsp_lines.nvim',
  { 'nvim-treesitter/nvim-treesitter', build = ":TSUpdate" }, -- very important
  { 'nvim-treesitter/playground' },
  { "nvim-tree/nvim-web-devicons", lazy = true },
  { "catppuccin/nvim", name = "catppuccin", priority = 1000 },
  'weilbith/nvim-code-action-menu',
  'gpanders/nvim-parinfer',
  'bakpakin/fennel.vim',
  'rktjmp/fwatch.nvim',
  { 'echasnovski/mini.nvim', branch = 'stable' },
}
