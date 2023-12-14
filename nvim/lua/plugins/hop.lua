return {
  'smoka7/hop.nvim',

  branch = 'v2', -- optional but strongly recommended
  config = function()
    local keys = require('core.keymap')
    -- you can configure Hop the way you like here; see :h hop-config
    require 'hop'.setup { keys = 'etovxqpdygfblzhckisuran' }

    -- vim.keymap.set('n', '<leader><space>', ':HopWord<CR>')
    keys.map('<leader><space>', 'hop around',
      ':HopWord<CR>',
     { mode = 'n' })
  end
}
