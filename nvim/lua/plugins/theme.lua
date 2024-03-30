return {
  -- "catppuccin/nvim",
  -- name = "catppuccin",
  {
    "olimorris/onedarkpro.nvim",
    priority = 1000,
    config = function()
      require('modus-themes').setup {
        on_highlights = function(hl, c)
          hl.NeogitDiffContext = {
            bg = c.bg_dim,
            fg = c.fg_main,
          }
          hl.NeogitHunkHeader = {
            bg = c.bg_alt,
            fg = c.fg_main,
          }

          hl.NeogitCursorLine = {
            bg = c.none,
          }
        end
      }
    end
  },

  { "miikanissi/modus-themes.nvim", priority = 1000 }
}
