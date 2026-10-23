local keys = require('core.keymap')
local cmp_nvim_lsp = require('cmp_nvim_lsp')
local M = {}


M.setup = function()
  local capabilities = cmp_nvim_lsp.default_capabilities()
  vim.lsp.config('*', {
    capabilities = capabilities,
    keymap = {
      recommended = true,
      jump_to_mark = "<c-Tab>",
    },
    flags = {
      debounce_text_changed = 150,
    },
  })

  vim.api.nvim_create_autocmd('LspAttach', {
    group = vim.api.nvim_create_augroup('CoreLsp', { clear = true }),
    callback = function(event)
      local client = vim.lsp.get_client_by_id(event.data.client_id)
      local bufnr = event.buf
      local opts = { noremap = true, silent = true, buffer = bufnr }

      keys.map("gR", "Show LSP References", ":Telescope lsp_references<CR>", opts)
      keys.map("gD", "Go to declaration", vim.lsp.buf.declaration, opts)
      keys.map("gd", "Show Definitions", ":Telescope lsp_definitions<CR>", opts)
      keys.map("gi", "Show Implementation", ":Telesecope lsp_implementations<CR>", opts)
      keys.map("gt", "Show Type", ":Telesecope lsp_type_definitions<CR>", opts)
      keys.map("<leader>ca", "Show Code Actions", vim.lsp.buf.code_action, opts)
      keys.map("<leader>rn", "Rename Symbol", vim.lsp.buf.rename, opts)
      keys.map("<leader>D", "Show Buffer Diagnostics", ":Telescope diagnostics buffnr=0<CR>", opts)
      keys.map("<leader>d", "Show Line Diagnostics", vim.diagnostic.open_float, opts)
      keys.map("K", "Show documentation", vim.lsp.buf.hover, opts)
      keys.map("<c-k>", "Show Signature Help", vim.lsp.buf.signature_help, opts)
      keys.map("<leader>rs", "Restart LSP", ":LspRestart<CR>", opts)

      if client.name == "clangd" then
        keys.map("<leader>f", "Clang Format", ":ClangFormat<cr>", opts)
      else
        keys.map("<leader>f", "Format", vim.lsp.buf.format, opts)
      end
    end
  })
end

return M
