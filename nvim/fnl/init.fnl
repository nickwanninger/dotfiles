(require :plugins)
(require :core)

(local keys (require :core.keymap))

;; setup neovimtree as a replacement for nerd tree
(let [{: setup} (require :nvim-tree)]
  (setup {:view {:side :left
                 :width 30
                 ;; :auto_resize true
                 :hide_root_folder true}
          ;; :auto_close true
          :disable_netrw true
          :hijack_netrw true
          :hijack_cursor true
          :open_on_tab true
          :update_cwd true
          :git {:enable false :ignore true}
          :hijack_directories {:enable true :auto_open true}
          ;; :actions {:open_file {:resize_window true}}
          :renderer {:indent_markers {:enable false}}}))


;; (let [{: setup} (require :gitsigns)] (setup))

(local ts (require :nvim-treesitter))
(local tsq (require :nvim-treesitter.query))
(local tsp (require :nvim-treesitter.parsers))

(let [{: setup} (require :nvim-treesitter.configs)]
  ;; Usual setup for treesitter
  (setup {:ensure_installed [ "c" "cpp" "fennel" "rust"]
          :ensure_maintained "maintained"
          :sync_install false
          :indent {:enable true}
          :incremental_selection {:enable true}
          :highlight { :enable true
                       :additional_vim_regex_highlighting false
                       :indent {:enable true}}}))


(set vim.notify (require :notify))
(vim.notify.setup {:background_colour "#000000"})
                   

;; Setup nvim-comment
(let [{: setup} (require :nvim_comment)]
  (setup {:line_mapping "<leader>cl"}))

;; TODO: stop using vim.cmd
(vim.cmd "syntax enable")
(vim.cmd "colorscheme snazzy")

(vim.cmd "set shell=fish")

;; (vim.cmd "let g:airline_theme='xcode'")
(vim.cmd "hi Normal ctermbg=NONE guibg=NONE")
(vim.cmd "hi NvimTreeNormal guibg=#090909")
(vim.cmd "highlight VertSplit guibg=NONE guifg=#111111")

(vim.cmd "autocmd TermOpen * setlocal nonumber norelativenumber")

;; Hide the lines in the splits
;; (vim.cmd "set fillchars+=vert:\\ ")
