return {
  "folke/persistence.nvim",
  -- event = "BufReadPre",
  config = function()
    require 'persistence'.setup {}


    local keys = require('core.keymap')

    keys.map('<leader>s', 'Open the most recent session',
      function()
        require 'persistence'.load()
      end
      , { mode = 'n' })
  end
}
