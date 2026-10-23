return {
  'williamboman/mason.nvim',
  dependencies = {
    'williamboman/mason-lspconfig.nvim',
    'neovim/nvim-lspconfig',
  },
  config = function()
    local mason = require('mason')
    local mason_lspconfig = require('mason-lspconfig')

    mason.setup {}
    require('core.lsp').setup()
    mason_lspconfig.setup {
      ensure_installed = {
        'rust_analyzer',
        'pyright',
      },
      automatic_enable = { exclude = { "clangd" } }
    }

    vim.lsp.enable('clangd')
  end
}
