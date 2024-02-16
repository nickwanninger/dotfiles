return {
  "jackMort/ChatGPT.nvim",
  event = "VeryLazy",
  dependencies = {
    "MunifTanjim/nui.nvim",
    "nvim-lua/plenary.nvim",
    "folke/trouble.nvim",
    "nvim-telescope/telescope.nvim"
  },
  config = function()
    local home = vim.fn.expand("$HOME")

    require("chatgpt").setup {
      api_key_cmd = "cat " .. home .. "/.openai.key"
    }
  end,
}
