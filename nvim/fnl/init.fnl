(require :plugins)
(require :core)

(local keys (require :core.keymap))

;; setup neovimtree as a replacement for nerd tree
(let [{: setup} (require :nvim-tree)]
  (setup {:view {:side :left
                 :width 30}
          :filters {:dotfiles true}
          :disable_netrw true
          :hijack_netrw true
          :hijack_cursor true
          :open_on_tab true
          :update_cwd true
          :git {:enable true :ignore true}
          :hijack_directories {:enable true :auto_open true}
          :renderer {:indent_markers {:enable false}}}))


(let [{: setup} (require :gitsigns)]
  (setup
    {:current_line_blame true
     :numhl false
     :word_diff false}))
     

(local ts (require :nvim-treesitter))
(local tsq (require :nvim-treesitter.query))
(local tsp (require :nvim-treesitter.parsers))

(let [{: setup} (require :nvim-treesitter.configs)]
  ;; Usual setup for treesitter
  (setup {:ensure_installed [ "c" "cpp" "fennel" "rust"]
          :ensure_maintained "maintained"
          :sync_install false
          ;; :indent {:enable true}
          :incremental_selection {:enable true}
          :highlight { :enable true
                       :additional_vim_regex_highlighting false
                       :indent {:enable true}}}))


(set vim.notify (require :notify))
(vim.notify.setup {})

;; Setup nvim-comment
(let [{: setup} (require :Comment)]
  (setup))

;; TODO: stop using vim.cmd
(vim.cmd "syntax enable")
;; (vim.cmd "colorscheme snazzy")

(vim.cmd "set shell=fish")

(vim.cmd "let $FZF_DEFAULT_OPTS = '--reverse'")
(vim.cmd "autocmd TermOpen * setlocal nonumber norelativenumber")

(local colors (require :core.colors))


(local darktheme colors.snazzy-black)
; (local darktheme colors.carbon-black)
(local lighttheme colors.one-light)


(keys.map "<leader>1" "Select Dark Theme"
  (fn [] (colors.select-colorscheme darktheme))
  {})

(keys.map "<leader>2" "Select Light Theme"
  (fn [] (colors.select-colorscheme lighttheme))
  {})


(keys.map "<leader>3" "Zen mode"
  (fn [] (vim.cmd ":ZenMode"))
  {})

(colors.select-colorscheme darktheme)
