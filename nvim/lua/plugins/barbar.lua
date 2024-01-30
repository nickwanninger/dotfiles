return {
  {
    'romgrk/barbar.nvim',
    dependencies = {
      'lewis6991/gitsigns.nvim',     -- OPTIONAL: for git status
      'nvim-tree/nvim-web-devicons', -- OPTIONAL: for file icons
    },
    version = '^1.0.0',              -- optional: only update when a new 1.x version is released
    config = function()
      vim.g.barbar_auto_setup = false


      local keys = require('core.keymap')


      require 'barbar'.setup {
        auto_hide = 1,
      }


      -- J and K are to move left and right
      keys.map('<M-j>', 'Previous tab', ':BufferPrevious<CR>', { mode = 'n' })
      keys.map('<M-k>', 'Previous tab', ':BufferNext<CR>', { mode = 'n' })

      -- Hold shift to move
      keys.map('<M-S-j>', 'Move tab left', ':BufferMovePrevious<CR>', { mode = 'n' })
      keys.map('<M-S-k>', 'Move tab right', ':BufferMoveNext<CR>', { mode = 'n' })
    end
  },
}
