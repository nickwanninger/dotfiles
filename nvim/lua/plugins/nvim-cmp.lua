return {
  'hrsh7th/nvim-cmp',
  event = 'InsertEnter',
  dependencies = {
    'hrsh7th/cmp-buffer', -- source for text buffer
    'hrsh7th/cmp-path', -- source for filesystem path
    'L3MON4D3/LuaSnip', -- snippet engine
    'saadparwaiz1/cmp_luasnip', -- for autocompletion
    'rafamadriz/friendly-snippets', -- useful snippets
  },

  config = function()
    local cmp = require('cmp')

    local luasnip = require('luasnip')
    local luasnip_from_vscode = require('luasnip.loaders.from_vscode')

    luasnip_from_vscode.lazy_load()
    cmp.setup {
      completion = {completeopt = "menu,menuone,preview,insert"},
      experimental = {
        ghost_text = true
      },
      snippet = {
        expand = function(args)
          luasnip.lsp_expand(args.body)
        end
      },
      mapping = cmp.mapping.preset.insert {
        ["<C-k>"] = cmp.mapping.select_prev_item(),
        ["<C-j>"] = cmp.mapping.select_next_item(),
        ["<C-b>"] = cmp.mapping.scroll_docs(-4),
        ["<C-f>"] = cmp.mapping.scroll_docs(4),
        ["<C-e>"] = cmp.mapping.abort(),
        ["<CR>"]  = cmp.mapping.confirm({select = true})
      },
      sources = cmp.config.sources{
        {name = "nvim_lsp"},
        {name = "luasnip"},
        {name = "buffer"},
        {name = "path"}
      }
    }

  end
}