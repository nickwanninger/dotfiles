return {
  {
    'nvim-treesitter/nvim-treesitter',
    build = ':TSUpdate',
    config = function()
      require('nvim-treesitter.configs').setup {
        ensure_maintained = 'maintained',
        sync_install = true,
        indent = {
          enable = true,
        },
        incremental_selection = {
          enable = true,
          keymaps = {
            init_selection = '.',
            scope_incremental = '<CR>',
            node_incremental = '.',
            node_decremental = ',',
          }
        },
        highlight = {
          enable = true,
          additional_vim_regex_highlighting = false,
          indent = { enable = true },
        },

      }
    end
  },
  {
    'nvim-treesitter/playground',
    dependencies = {
      'nvim-treesitter/nvim-treesitter'
    }
  }
}
