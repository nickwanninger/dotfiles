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

{: set-lsp-keys!
 : map}
