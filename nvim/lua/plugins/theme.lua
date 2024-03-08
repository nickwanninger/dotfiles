return {
  -- "catppuccin/nvim",
  -- name = "catppuccin",


  "olimorris/onedarkpro.nvim",


  priority = 1000,
  config = function()

    local wk = require('which-key')

    -- local theme_name = 'catppuccin-mocha'
    -- theme_name = 'monokai-pro-spectrum'
    local theme_name = 'onedark_dark'
    vim.cmd('colorscheme ' .. theme_name)

    wk.register {
      ['<leader>1'] = {
        function()
          vim.cmd('colorscheme ' .. theme_name)
        end,
        "Dark Theme"
      },
      ['<leader>2'] = {
        function()
          vim.cmd('colorscheme base16-gruvbox-material-light-soft')
        end,
        "Light Theme"
      }
    }
  end
}
