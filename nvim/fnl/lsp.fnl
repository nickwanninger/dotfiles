(local keys (require :core.keymap))

(local lsps ["clangd" "pyright" "rust_analyzer"])


;;; Setup Mason
(local mason (require :mason))
(local mason-lspconfig (require :mason-lspconfig))
(mason.setup {})
(mason-lspconfig.setup
    {:ensure_installed lsps
     :automatic_installation true})
                      


;; Configure the lsp server
(local lsp (require :lspconfig))
(local cmp-nvim-lsp (require :cmp_nvim_lsp))
(local capabilities (cmp-nvim-lsp.default_capabilities))

(fn on-attach [client bufnr]
  (let [opts {:noremap true :silent true :buffer bufnr}
        map (fn [combo help cmd]
              (keys.map combo help cmd opts))]
    (map "gR" "Show LSP References" ":Telescope lsp_references<CR>")
    (map "gD" "Go to declaration" vim.lsp.buf.declaration)
    (map "gd" "Show Definitions" ":Telescope lsp_definitions<CR>")
    (map "gi" "Show Implementation" ":Telesecope lsp_implementations<CR>")
    (map "gt" "Show Type" ":Telesecope lsp_type_definitions<CR>")
    (map "<leader>ca" "Show Code Actions" vim.lsp.buf.code_action)
    (map "<leader>rn" "Rename Symbol" vim.lsp.buf.rename)
    (map "<leader>D" "Show Buffer Diagnostics" ":Telescope diagnostics buffnr=0<CR>")
    (map "<leader>d" "Show Line Diagnostics" vim.diagnostic.open_float)
    (map "K" "Show documentation" vim.lsp.buf.hover)
    (map "<c-k>" "Show Signature Help" vim.lsp.buf.signature_help)
    (map "<leader>rs" "Restart LSP" ":LspRestart<CR>")))

; ;; Go over the different language servers that I want to use and configure them
; (let [servers lsps] ; ["clangd" "rust_analyzer" "hls"]]
;   (each [_ lang (pairs servers)]
;     (let [{: setup} (. lsp lang)]
;       (setup {:capabilities capabilities
;               :on_attach on-attach}))))

(mason-lspconfig.setup_handlers 
  [(fn [server_name]
     (let [{: setup} (. lsp server_name)]
        (setup {:capabilities capabilities
                :on_attach on-attach})))])




;; Then setup cmp
(local cmp (require :cmp))
(local luasnip (require :luasnip))
(local luasnip-from-vscode (require :luasnip.loaders.from_vscode))

(luasnip-from-vscode.lazy_load)
(cmp.setup {:completion {:completeopt "menu,menuone,preview,insert"}
            :experimental {:ghost_text true}
            :snippet {:expand (fn [args]
                                (luasnip.lsp_expand args.body))}
            :mapping (cmp.mapping.preset.insert
                      {"<C-k>" (cmp.mapping.select_prev_item)
                       "<C-j>" (cmp.mapping.select_next_item)
                       "<C-b>" (cmp.mapping.scroll_docs -4)
                       "<C-f>" (cmp.mapping.scroll_docs 4)
                       "<Tab>" (cmp.mapping.complete)
                       "<C-e>" (cmp.mapping.abort)
                       ;; "<Tab>" (cmp.mapping.confirm {:select true})
                       "<CR>"  (cmp.mapping.confirm {:select true})})
            :sources (cmp.config.sources
                       [{:name "nvim_lsp"}
                        {:name "luasnip"}
                        {:name "buffer"}
                        {:name "path"}])})





