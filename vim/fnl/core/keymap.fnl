(local which-key (require :which-key))

(fn key [tbl prop] [(. tbl prop) prop])

(which-key.register {";" [":" "vim-ex"]})


(fn keybind [bufnr bindings]
  (which-key.register bindings {:buffer bufnr}))


(fn set-lsp-keys! [bufnr]
  (keybind bufnr { "gd" (key vim.lsp.buf :definition)})
  (keybind bufnr { "gD" (key vim.lsp.buf :declaration)})
  (keybind bufnr { "gi" (key vim.lsp.buf :implementation)})
  (keybind bufnr { "K"  (key vim.lsp.buf :hover)})
  (keybind bufnr {"<leader>d" {:name "lsp"
                               "k" (key vim.diagnostic :goto_prev)
                               "j" (key vim.diagnostic :goto_next)
                               "w" (key vim.diagnostic :open_float)}}))
                

;;(which-key.register {"<leader>d" {:name "lsp"
;;                                  ; inspect
;;                                  "d" (key vim.lsp.buf :definition)
;;                                  "D" (key vim.lsp.buf :declaration)
;;                                  "i" (key vim.lsp.buf :implementation)
;;                                  "t" (key vim.lsp.buf :type_definition)
;;                                  "s" (key vim.lsp.buf :signature_help)
;;                                  "h" (key vim.lsp.buf :hover)
;;                                  "r" (key vim.lsp.buf :references)
;;                                  ; diagnstic
;;                                  "k" (key vim.diagnostic :goto_prev)
;;                                  "j" (key vim.diagnostic :goto_next)
;;                                  "w" (key vim.diagnostic :open_float)
;;                                  "q" (key vim.diagnostic :setloclist)
;;                                  ; code
;;                                  "r" (key vim.lsp.buf :rename)
;;                                  "a" (key vim.lsp.buf :code_action)
;;                                  "f" (key vim.lsp.buf :formatting))
;;                     "<leader>W" {:name "lsp workspace"
;;                                  "a" (key vim.lsp.buf :add_workspace_folder)
;;                                  "r" (key vim.lsp.buf :remove_workspace_folder)
;;                                  "l" [(fn [] (print (vim.inspect (vim.lsp.buf.list_workspace_folders))))
;;                                       "list_workspace_folders"]}
;;                     ; reassgn some builtin mappings
;;                     "K"  (key vim.lsp.buf :hover)
;;                     "gd" (key vim.lsp.buf :definition)
;;                     "gD" (key vim.lsp.buf :declaration)}
;;              ; only for one buffer
;;              {:buffer bufnr}))


{: set-lsp-keys!}
