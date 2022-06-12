;; The name is up to you.
(module nvim-config
  {;; You can use Lua's regular require or Aniseed's autoload.
   require {: coq
            : gitsigns
            : nvim-tree
            : lspconfig
            treesitter nvim-treesitter.configs}
    
   

            ;; Fennel destructuring syntax works but defeats the point of autoload.
            ;; Because a lookup is instantly invoked which triggers autoload at
            ;; module load time instead of when you need it in your code.
            ; {: some-fn} some-module}
    autoload {a aniseed.core}})


;; setup gitsigns with no custom configuration
(gitsigns.setup {})
;; Setup NeovimTree as a replacement for nerd tree
(nvim-tree.setup {})

;; Configure the language server configuration with bindings per supported buffer
(fn on-attach [client bufnr]
  (let [opts {:noremap true :silent true}]
     (vim.api.nvim_buf_set_keymap bufnr "n" "gd" "<cmd>lua vim.lsp.buf.definition()<CR>" opts)
     (vim.api.nvim_buf_set_keymap bufnr "n" "K" "<cmd>lua vim.lsp.buf.hover()<CR>" opts)
     (vim.api.nvim_buf_set_keymap bufnr "n" "gi" "<cmd>lua vim.lsp.buf.implementation()<CR>" opts)
     (vim.api.nvim_buf_set_keymap bufnr "n" "<C-k>" "<cmd>lua vim.lsp.buf.signature_help()<CR>" opts)
     (vim.api.nvim_buf_set_keymap bufnr "n" "rn" "<cmd>lua vim.lsp.buf.rename()<CR>" opts)
     (vim.api.nvim_buf_set_keymap bufnr "n" "<leader>ca" "<cmd>lua vim.lsp.buf.code_action()<CR>" opts)
     (vim.api.nvim_buf_set_keymap bufnr "n" "gr" "<cmd>lua vim.lsp.buf.references()<CR>" opts)
     (vim.api.nvim_buf_set_keymap bufnr "n" "<leader>f" "<cmd>lua vim.lsp.buf.formatting()<CR>" opts)))

;;; (a.println "Welcome")

;; Go over the different language servers that I want to use and configure them
(let [servers ["clangd" "pyright" "rust_analyzer"]]
  (each [_ lsp (pairs servers)]
    (let [setup (. lspconfig lsp :setup)]
      (setup { :on_attach on-attach 
               :keymap { :recommended true
                         :jump_to_mark "<c-Tab>"}
               :flags { :debounce_text_changed 150}}))))


(treesitter.setup
 {:ensure_maintained "maintained"
  :sync_install false
  :highlight { ;; False wll disable the extension
                :enable true
                ;; Setting this to true will run `:h syntax` and tree-sitter at the same time.
                ; Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
                ; Using this option may slow down your editor, and you may see some duplicate highlights.
                ; Instead of true it can also be a list of languages
                :additional_vim_regex_highlighting false}})

(defn nnoremap [from to opts]
  (let [map-opts {:noremap true}
        to (.. ":" to "<cr>")]
    (if (a.get opts :local?)
      (nvim.buf_set_keymap 0 :n from to map-opts)
      (nvim.set_keymap :n from to map-opts))))
   
;; Begin COQ
(vim.cmd "COQnow -s")
