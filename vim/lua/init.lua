local _2afile_2a = "/Users/nick/.config/nvim/fnl/init.fnl"
local _2amodule_name_2a = "nvim-config"
local _2amodule_2a
do
  package.loaded[_2amodule_name_2a] = {}
  _2amodule_2a = package.loaded[_2amodule_name_2a]
end
local _2amodule_locals_2a
do
  _2amodule_2a["aniseed/locals"] = {}
  _2amodule_locals_2a = (_2amodule_2a)["aniseed/locals"]
end
local autoload = (require("aniseed.autoload")).autoload
local a, coq, gitsigns, lspconfig, nvim_tree, treesitter = autoload("aniseed.core"), require("coq"), require("gitsigns"), require("lspconfig"), require("nvim-tree"), require("nvim-treesitter.configs")
do end (_2amodule_locals_2a)["a"] = a
_2amodule_locals_2a["coq"] = coq
_2amodule_locals_2a["gitsigns"] = gitsigns
_2amodule_locals_2a["lspconfig"] = lspconfig
_2amodule_locals_2a["nvim-tree"] = nvim_tree
_2amodule_locals_2a["treesitter"] = treesitter
gitsigns.setup({})
nvim_tree.setup({})
local function on_attach(client, bufnr)
  local opts = {noremap = true, silent = true}
  vim.api.nvim_buf_set_keymap(bufnr, "n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "K", "<cmd>lua vim.lsp.buf.hover()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "<C-k>", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "rn", "<cmd>lua vim.lsp.buf.rename()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "<leader>ca", "<cmd>lua vim.lsp.buf.code_action()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
  return vim.api.nvim_buf_set_keymap(bufnr, "n", "<leader>f", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
end
a.println("Welcome")
do
  local servers = {"clangd", "pyright", "rust_analyzer"}
  for _, lsp in pairs(servers) do
    local setup = lspconfig[lsp].setup
    setup({on_attach = on_attach, keymap = {recommended = true, jump_to_mark = "<c-Tab>"}, flags = {debounce_text_changed = 150}})
  end
end
treesitter.setup({ensure_maintained = "maintained", sync_install = false, highlight = {enable = true, additional_vim_regex_highlighting = false}})
local function nnoremap(from, to, opts)
  local map_opts = {noremap = true}
  local to0 = (":" .. to .. "<cr>")
  if a.get(opts, "local?") then
    return nvim.buf_set_keymap(0, "n", from, to0, map_opts)
  else
    return nvim.set_keymap("n", from, to0, map_opts)
  end
end
_2amodule_2a["nnoremap"] = nnoremap
vim.cmd("COQnow -s")
return _2amodule_2a