(local wk (require :which-key))

(wk.setup {})

(fn key [tbl prop] [(. tbl prop) prop])

(wk.register {";" [":" "vim-ex"]})



(fn keybind [bufnr bindings]
  (wk.register bindings {:buffer bufnr}))


(fn set-lsp-keys! [bufnr]
  (keybind bufnr { "gd" (key vim.lsp.buf :definition)})
  (keybind bufnr { "gD" (key vim.lsp.buf :declaration)})
  (keybind bufnr { "gi" (key vim.lsp.buf :implementation)})
  (keybind bufnr { "K"  (key vim.lsp.buf :hover)})
  (keybind bufnr {"<leader>d" {:name "lsp"
                               "k" (key vim.diagnostic :goto_prev)
                               "j" (key vim.diagnostic :goto_next)
                               "w" (key vim.diagnostic :open_float)}}))
                


(lambda map [binding name func ?opt]
  (let [opt (or ?opt {})]
    (wk.register { binding [func name]} opt)))



(map "<C-n>" "Focus on the tree view" ":NvimTreeToggle<CR>" {:mode "n"})
(map "<C-S-n>" "Focus on the tree view" ":NvimTreeFocus<CR>" {:mode "n"})
(map "<M-n>" "Close the tree view" ":NvimTreeClose<CR>" {:mode "n"})

(map "?" "Display keymaps" ":WhichKey<CR>" {:mode "n"})
(map "<C-f>" "Display git files" ":GFiles<CR>" {:mode "n"})
(map "<C-p>" "Display all files" ":Files<CR>" {:mode "n"})
(map "<leader>ca" "Open display code actions" ":CodeActionMenu" {:mode "n"})
(map "<leader>f" "Clang Format" ":ClangFormat<CR>" {:silent true :mode "n"})



;; Some sane split commands
(map "<C-_>" "Horizontal Split" ":sp<CR>" {:mode "n"})
(map "<C-\\>" "Vertical Split" ":vsp<CR>" {:mode "n"})

(map "<C-q>" "Close the current split" ":q<CR>" {:mode "n"})

(map "<" "Dedent" "<gv" {:mode "v"})
(map ">" "Indent" ">gv" {:mode "v"})




(map "qq" "exit" ":q<CR>")

(map "<space>" "Select the word under the cursor" "<ESC>viw")

(map "<leader>P" "Run PackerSync" ":PackerSync"
     {:silent true
      :mode "n"})

;; Unmap recording... It's annoying if you don't use it
(vim.cmd "nnoremap <silent> Q q")
(vim.cmd "nnoremap <silent> q <Nop>")


              

{: set-lsp-keys!
 : map}
