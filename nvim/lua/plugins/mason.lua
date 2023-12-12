return {
  'williamboman/mason.nvim',
  dependencies = {
    'williamboman/mason-lspconfig.nvim',
    'neovim/nvim-lspconfig',
  },
  config = function()
    local mason = require('mason')
    local mason_lspconfig = require('mason-lspconfig')

    local lsp = require('lspconfig')
    local core_lsp = require('core.lsp')

    mason.setup {}
    mason_lspconfig.setup {
      ensure_installed = {'clangd', 'rust_analyzer'},
      automatic_installation = true,
    }

    mason_lspconfig.setup_handlers {
      core_lsp.setup_server
    }
  end
}