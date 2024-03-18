return {
  -- "catppuccin/nvim",
  -- name = "catppuccin",


  "olimorris/onedarkpro.nvim",


  priority = 1000,
  config = function()

    local wk = require('which-key')

    require("onedarkpro").setup({
      colors = {
        onedark_dark = { fg = "#FFFFFF" },
        onelight = { fg = '#000000', bg = "#ffffff" }, -- green
      }
    })

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
          vim.cmd('colorscheme onelight')
        end,
        "Light Theme"
      }
    }
  end
}
