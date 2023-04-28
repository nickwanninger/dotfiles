(local lsp (require :lspconfig))
(local {: set-lsp-keys!} (require :core.keymap))
(local coq (require :coq))

;; 
(fn on-attach [client bufnr]
  (let [opts {:noremap true :silent true}]
    ;; (set-lsp-keys! bufnr)
    (vim.api.nvim_buf_set_keymap bufnr "n" "gd" "<cmd>lua vim.lsp.buf.definition()<CR>" opts)
    (vim.api.nvim_buf_set_keymap bufnr "n" "K" "<cmd>lua vim.lsp.buf.hover()<CR>" opts)
    (vim.api.nvim_buf_set_keymap bufnr "n" "gi" "<cmd>lua vim.lsp.buf.implementation()<CR>" opts)
    (vim.api.nvim_buf_set_keymap bufnr "n" "<C-k>" "<cmd>lua vim.lsp.buf.signature_help()<CR>" opts)
    (vim.api.nvim_buf_set_keymap bufnr "n" "rn" "<cmd>lua vim.lsp.buf.rename()<CR>" opts)
    (vim.api.nvim_buf_set_keymap bufnr "n" "<leader>ca" "<cmd>lua vim.lsp.buf.code_action()<CR>" opts)
    (vim.api.nvim_buf_set_keymap bufnr "n" "gr" "<cmd>lua vim.lsp.buf.references()<CR>" opts)
    (vim.api.nvim_buf_set_keymap bufnr "n" "<leader>f" "<cmd>lua vim.lsp.buf.format()<CR>" opts)))


;; Go over the different language servers that I want to use and configure them
(let [servers ["clangd" "pyright" "rust_analyzer"]]
  (each [_ lang (pairs servers)]
    (let [{: setup} (. lsp lang)]
      (setup (coq.lsp_ensure_capabilities { :on_attach on-attach} 
                  :keymap  {:recommended true :jump_to_mark "<c-Tab>"}
                  :flags   {:debounce_text_changed 150})))))

;; Start the lsp
(coq.Now "-s")

(let [{: setup} (require :lsp_lines)]
  (setup))

;Disable virtual_text since it's redundant due to lsp_lines.
(vim.diagnostic.config
  {:virtual_text false})

