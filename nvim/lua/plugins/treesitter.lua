return {
  {
    'nvim-treesitter/nvim-treesitter',
    branch = 'main',
    lazy = false,
    build = ':TSUpdate',
    config = function()
      require('nvim-treesitter').setup {}

      vim.api.nvim_create_autocmd('FileType', {
        group = vim.api.nvim_create_augroup('TreesitterHighlight', { clear = true }),
        callback = function(event)
          local ok, parser = pcall(vim.treesitter.get_parser, event.buf)
          if not ok or not parser then
            return
          end

          vim.treesitter.start(event.buf)

          -- Neovim 0.12.2 does not provide native node selection.
          if vim.treesitter.select then
            vim.keymap.set({ 'n', 'x' }, '.', function()
              vim.treesitter.select('parent', vim.v.count1)
            end, { buffer = event.buf, desc = 'Expand node selection' })
            vim.keymap.set('x', ',', function()
              vim.treesitter.select('child', vim.v.count1)
            end, { buffer = event.buf, desc = 'Shrink node selection' })
          end
        end,
      })
    end
  },
}
