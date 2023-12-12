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
      require('tangerine').setup {
       -- Start by using the init.fnl file
       vimrc   = vim.fn.stdpath [[config]] .. "/fnl/init.fnl",
       -- And set the 'include path' for fennel files to $RT/fnl
       source  = vim.fn.stdpath [[config]] .. "/fnl",
       target = vim.fn.stdpath [[data]] .. "/tangerine",
       compiler = {
         verbose = false,
         hooks = { "oninit", "onsave" },
       }
      }
    end
  },

  { "nvim-neo-tree/neo-tree.nvim",
    branch = "v3.x",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-tree/nvim-web-devicons", -- not strictly required, but recommended
      "MunifTanjim/nui.nvim",
      -- "3rd/image.nvim", -- Optional image support in preview window: See `# Preview Mode` for more information
    }
  },

  "folke/which-key.nvim",
  'folke/zen-mode.nvim',
  { "NeogitOrg/neogit",
    dependencies = {
      "nvim-lua/plenary.nvim",         -- required
      "nvim-telescope/telescope.nvim", -- optional
      "sindrets/diffview.nvim",        -- optional
      "ibhagwan/fzf-lua",              -- optional
    },
  },
  {
    'stevearc/dressing.nvim',
    opts = {},
  },
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
  -- 'kyazdani42/nvim-tree.lua',
  'rcarriga/nvim-notify',
  'voldikss/vim-floaterm',


  { 'williamboman/mason.nvim',
    dependencies = {
      'williamboman/mason-lspconfig.nvim'
    }
  },
  -- 'ms-jpq/coq_nvim',
  -- 'ms-jpq/coq.artifacts',
  -- 'ray-x/lsp_signature.nvim',
  -- 'ErichDonGubler/lsp_lines.nvim',
  
  { 'neovim/nvim-lspconfig',
    dependencies = {
     "hrsh7th/cmp-nvim-lsp",
     { "antosha417/nvim-lsp-file-operations", config = true },
    },
  },
  { 'hrsh7th/nvim-cmp',
    event = 'InsertEnter',
    dependencies = {
     'hrsh7th/cmp-buffer', -- source for text buffer
     'hrsh7th/cmp-path', -- source for filesystem path
     'L3MON4D3/LuaSnip', -- snippet engine
     'saadparwaiz1/cmp_luasnip', -- for autocompletion
     'rafamadriz/friendly-snippets', -- useful snippets
    },
  },


  'rhysd/vim-clang-format',
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
