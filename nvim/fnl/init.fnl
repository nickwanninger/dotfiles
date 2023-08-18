(local keys (require :keymap))

(require :lsp)
(require :statusline)

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
  (setup {:ensure_installed [ "c" "cpp" "fennel" "rust" "racket"]
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

(local colors (require :highlight))


; (local darktheme colors.snazzy-black)
(local darktheme colors.nord-black)
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


(fn run-buffer-with [program]
  (vim.cmd (.. ":FloatermNew --autoclose=0 --height=0.9 --width=0.9 " program " %")))
 
;; Toggle cursor "target"
(keys.map "<leader>l"
          "Toggle Cursor Target"
          (fn []
            (do
              (vim.cmd "set cursorline!")
              (vim.cmd "set cursorcolumn!")))
          {})

;; Run certain programs
(keys.map "<leader>r"
     "run the current file"
     (fn [] 
       (let [buf (vim.api.nvim_get_current_buf)
             ft (vim.api.nvim_buf_get_option buf "filetype")]
         (if (= ft "racket") (run-buffer-with "racket"))
         (if (= ft "scheme") (run-buffer-with "racket")))))



;; Remove the default menu items
(vim.cmd "aunmenu PopUp.How-to\\ disable\\ mouse")
(vim.cmd "aunmenu PopUp.-1-")

(vim.cmd.anoremenu "PopUp.-1- :")
(vim.cmd.anoremenu "PopUp.Go\\ to\\ definition gd")
(vim.cmd.anoremenu "PopUp.Go\\ to\\ declaration gD")
(vim.cmd.anoremenu "PopUp.Go\\ to\\ implementation gi")
(vim.cmd.anoremenu "PopUp.More\\ info K")

(vim.cmd.anoremenu "PopUp.-2- :")

(vim.cmd.anoremenu "PopUp.VSplit :vsp<CR>")
(vim.cmd.anoremenu "PopUp.HSplit :sp<CR>")
(vim.cmd.anoremenu "PopUp.-3- :")
(vim.cmd.anoremenu "PopUp.Save :w<CR>")
(vim.cmd.anoremenu "PopUp.Save+Quit :q<CR>")
(vim.cmd.anoremenu "PopUp.Quit! :q!<CR>")
(vim.cmd.anoremenu "PopUp.-4- :")
(vim.cmd.anoremenu "PopUp.Format :ClangFormat<CR>")
(vim.cmd.anoremenu "PopUp.Zen :ZenMode<CR>")
