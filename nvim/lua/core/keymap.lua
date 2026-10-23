local wk = require('which-key')

wk.add {
  { ';', ':', desc = "vim-ex" }
}

local M = {}


M.map = function(binding, name, func, opt)
  opt = opt or {}
  wk.add {
    vim.tbl_extend('force', opt, { binding, func, desc = name })
  }
end


return M
