(local keys (require :keymap))
(local state (require :state))

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


;; Setup Neogit
(let [neogit (require :neogit)]
  ;; Setup neogit
  (neogit.setup {:kind "tab"})
  (keys.map "<leader>g"
            "Open neogit"
            neogit.open))

(keys.map "<C-S-Left>"  "Prev Tab" ":tabprev<CR>")
(keys.map "<C-S-Right>" "Next Tab" ":tabnext<CR>")
  


  




;; TODO: stop using vim.cmd
(vim.cmd "syntax enable")
;; (vim.cmd "colorscheme snazzy")

(vim.cmd "set shell=fish")

(vim.cmd "let $FZF_DEFAULT_OPTS = '--reverse'")
(vim.cmd "autocmd TermOpen * setlocal nonumber norelativenumber")

(vim.cmd "au BufRead,BufNewFile *.y set ft=haskell")
(vim.cmd "au BufRead,BufNewFile *.x set ft=haskell")
;; (vim.cmd "au BufRead,BufNewFile *.fz set syntax=haskell")
(vim.cmd "au BufRead,BufNewFile *.fz set syntax=futz")
(vim.cmd "au BufRead,BufNewFile *.fz set filetype=futz")
(vim.cmd "autocmd FileType futz setlocal commentstring=--%s")


(local colors (require :highlight))


;; (local darktheme colors.snazzy-black)
;; (local darktheme colors.atlas)
;; (local darktheme colors.nord-black)
(local darktheme colors.material-black)
(local lighttheme colors.one-light)

; (local darktheme colors.gruvbox-dark-hard)
; (local lighttheme colors.gruvbox-light-hard)

(state.register :colorscheme colors.select-colorscheme)
(state.default :colorscheme darktheme)

(keys.map "<leader>1" "Select Dark Theme"
  (fn [] (state.set-val :colorscheme darktheme))
  ;; (fn [] (colors.select-colorscheme darktheme))
  {})

(keys.map "<leader>2" "Select Light Theme"
  (fn [] (state.set-val :colorscheme lighttheme))
  ;; (fn [] (colors.select-colorscheme lighttheme))
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



(keys.map "<leader>h"
          "Open the Hoogle Search"
          (fn []
            (do
              (vim.cmd ":Hoogle")))
          {})


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



;; (state.write {:foo "hello"})


(state.start)
