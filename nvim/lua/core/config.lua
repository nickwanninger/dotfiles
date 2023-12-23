-- This file is called *after* lazy has finished configuring. It sets things up
M = {}


local keys = require('core.keymap')

M.setup = function(opts)
  vim.cmd("syntax enable")
  vim.cmd("set shell=fish")
  vim.cmd("let $FZF_DEFAULT_OPTS = '--reverse'")
  vim.cmd("autocmd TermOpen * setlocal nonumber norelativenumber")
  vim.cmd("au BufRead,BufNewFile *.y set ft=haskell")
  vim.cmd("au BufRead,BufNewFile *.x set ft=haskell")
  vim.cmd("au BufRead,BufNewFile *.fz set syntax=futz")
  vim.cmd("au BufRead,BufNewFile *.fz set filetype=futz")
  vim.cmd("autocmd FileType futz setlocal commentstring=--%s")

  vim.cmd("nnoremap <silent> Q q")
  vim.cmd("nnoremap <silent> q <Nop>")


  keys.map("<C-S-Left>", "Prev Tab", ":tabprev<CR>")
  keys.map("<C-S-Right>", "Next Tab", ":tabnext<CR>")

  keys.map("<C-n>", "Focus on the tree view", ":Neotree float<CR>", { mode = "n" })
  keys.map("?", "Display keymaps", ":WhichKey<CR>", { mode = "n" })
  keys.map("<M-f>", "Search", ":Telescope live_grep<CR>", { mode = "n" })
  keys.map("<C-f>", "Display git files", ":Telescope git_files<cr>", { mode = "n" })
  keys.map("<C-p>", "Display all files", ":Telescope find_files<CR>", { mode = "n" })
  keys.map("<C-_>", "Horizontal Split", ":sp<CR>", { mode = "n" })
  keys.map("<C-\\>", "Vertical Split", ":vsp<CR>", { mode = "n" })
  -- keys.map("<C-q>", "Close the current split", ":q<CR>", {mode = "n"})
  keys.map("<", "Dedent", "<gv", { mode = "v" })
  keys.map(">", "Indent", ">gv", { mode = "v" })
  keys.map("qq", "exit", ":q<CR>")
  keys.map("<space>", "Select the word under the cursor", "<ESC>viw")

  keys.map("<C-j>", "scroll down", "5j", { mode = "n" })
  keys.map("<C-k>", "scroll up", "4k", { mode = "n" })
  keys.map("<M-Left>", "Nav Left", ":TmuxNavigateLeft<cr>", { silent = true })
  keys.map("<M-Right>", "Nav Right", ":TmuxNavigateRight<cr>", { silent = true })
  keys.map("<M-Up>", "Nav Up", ":TmuxNavigateUp<cr>", { silent = true })
  keys.map("<M-Down>", "Nav Down", ":TmuxNavigateDown<cr>", { silent = true })
  keys.map("<leader>l", "Open lazy", ":Lazy<CR>")
  keys.map("<leader>ca", "Open display code actions", ":CodeActionMenu", { mode = "n" })
  keys.map("<leader>u", "Toggle Undotree", ":UndotreeToggle<cr>", { silent = true, mode = "n" })
  keys.map("<C-Bslash>", "Place a lambda", "\206\187", { mode = "i" })



  keys.map("<A-x>g", "Open Neogit", function ()
    require'neogit'.open()
  end)


  vim.wo.number = true
  keys.map("<C-c>", "Toggle line numbers", function()
    vim.wo.number = not vim.wo.number
  end)
end

-- vim.keymap.del('n', '<space>')


return M
