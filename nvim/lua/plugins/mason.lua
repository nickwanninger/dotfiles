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
    mason_lspconfig.setup {
      ensure_installed = {
        'rust_analyzer',
        'pyright',
      },
      automatic_installation = { exclude = { "clangd" } }
    }

    -- Next, hook up mason so that when a language server
    -- becomes available, it starts the server w/ lspconfig
    mason_lspconfig.setup_handlers {
      require'core.lsp'.setup_server
    }

    require('core.lsp').setup_server('clangd')


  end
}
