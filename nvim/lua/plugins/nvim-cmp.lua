return {
  'hrsh7th/nvim-cmp',
  event = 'InsertEnter',
  dependencies = {
    'hrsh7th/cmp-buffer',           -- source for text buffer
    'hrsh7th/cmp-path',             -- source for filesystem path
    'L3MON4D3/LuaSnip',             -- snippet engine
    'saadparwaiz1/cmp_luasnip',     -- for autocompletion
    'rafamadriz/friendly-snippets', -- useful snippets
  },

  config = function()
    local cmp = require('cmp')
    local wk = require('which-key')

    local luasnip = require('luasnip')
    local luasnip_from_vscode = require('luasnip.loaders.from_vscode')

    wk.register({
      ['<C-m>'] = { function() luasnip.jump(1) end, "Next mark" },
    })

    -- local keymap = vim.api.nvim_set_keymap
    -- local opts = { noremap = true, silent = true }
    -- keymap("i", "<c-j>", "<cmd>lua require'luasnip'.jump(1)<CR>", opts)
    -- keymap("s", "<c-j>", "<cmd>lua require'luasnip'.jump(1)<CR>", opts)
    -- keymap("i", "<c-k>", "<cmd>lua require'luasnip'.jump(-1)<CR>", opts)
    -- keymap("s", "<c-k>", "<cmd>lua require'luasnip'.jump(-1)<CR>", opts)

    luasnip_from_vscode.lazy_load()
    cmp.setup {
      completion = {
        completeopt = "menu,menuone,insert"
      },

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
        ["<C-h>"] = cmp.mapping.confirm { select = true },
        ["<Tab>"] = cmp.mapping(function(fallback)
          if cmp.visible() then
            cmp.select_next_item()
          elseif luasnip.expand_or_jumpable() then
            luasnip.expand_or_jump()
            -- elseif has_words_before() then
            --   cmp.complete()
          else
            fallback()
          end
        end, { "i", "s" }),

        ["<S-Tab>"] = cmp.mapping(function(fallback)
          if cmp.visible() then
            cmp.select_prev_item()
          elseif luasnip.jumpable(-1) then
            luasnip.jump(-1)
          else
            fallback()
          end
        end, { "i", "s" }),
      },
      sources = cmp.config.sources {
        { name = "nvim_lsp" },
        { name = "luasnip" },
        { name = "buffer" },
        { name = "path" }
      }
    }
  end
}
