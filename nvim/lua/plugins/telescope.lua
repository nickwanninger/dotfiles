return {
  'nvim-telescope/telescope.nvim',
  config = function()
    require("telescope").setup({
      defaults = {
        borderchars = { "█", " ", "▀", "█", "█", " ", " ", "▀" },
      }
    })
  end
}

