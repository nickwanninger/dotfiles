local wk = require('which-key')

wk.register {
  [';'] = {':', "vim-ex"}
}

M = {}


M.map = function(binding, name, func, opt)
  opt = opt or {}
  wk.register({
    [binding] = {func, name}
  }, opt)
end


return M