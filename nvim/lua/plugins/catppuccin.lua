return {
  "catppuccin/nvim",
  name = "catppuccin",
  priority = 1000,
  opts = {
    transparent_background = false
  },
  config = function()
    local wk = require('which-key')
    vim.cmd('colorscheme catppuccin-mocha')

    wk.register {
      ['<leader>1'] = {
        function()
          vim.cmd('colorscheme catppuccin-mocha')
        end,
        "Dark Theme"
      },
      ['<leader>2'] = {
        function()
          vim.cmd('colorscheme catppuccin-latte')
        end,
        "Light Theme"
      }
    }
  end
}
