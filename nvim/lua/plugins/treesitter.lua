return {
  {
    'nvim-treesitter/nvim-treesitter',
    build = ':TSUpdate',
    conifg = function()
      local ts = require('nvim-treesitter.configs')

      ts.setup {
        ensure_maintained = 'maintained',
        sync_install = false,
        indent = {
          enable = true,
        },
        incremental_selection = {
          enable = true,
        },
        highlight = {
          enable = true,
          additional_vim_regex_highlighting = false,
          indent = { enable = true },
        }
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