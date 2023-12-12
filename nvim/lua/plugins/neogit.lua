

return {
  "NeogitOrg/neogit",

  dependencies = {
    "nvim-lua/plenary.nvim",         -- required
    "nvim-telescope/telescope.nvim", -- optional
    "sindrets/diffview.nvim",        -- optional
    "ibhagwan/fzf-lua",              -- optional
  },
  config = function()
   local neogit = require('neogit')
   local keys = require('core.keymap')
   neogit.setup {
    disable_hint = true,
    disable_context_highlighting = true,
    disable_builtin_notifications = true,
    status = {
      recent_commit_count = 30
    },
    integrations = {
      diffview = true,
    },
    sections = {
      recent = {
        folded = false,
      },
    },
    mappings = {
      popup = {
        p = false
      }
    },
    disable_signs = false
  }


  keys.map("<leader>g", "Open Neogit", neogit.open)


  end
}
