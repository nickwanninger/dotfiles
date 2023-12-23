return {
  'rcarriga/nvim-notify',
  config = function()
    vim.notify = require('notify')
    vim.notify.setup {
      render = 'compact',
      minimum_width = 25,
      stages = 'slide',
      timeout = 1000,
    }
  end
}
