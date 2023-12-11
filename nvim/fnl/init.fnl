(local keys (require :core.keymap))
(local state (require :core.state))
(local {: setup} (require :core.setup))
(local io (require :core.io))

(require :lsp)
(require :statusline)



(setup :nvim-tree
  {:view {:side :left
          :width 30}
   :filters {:dotfiles true}
   :disable_netrw true
   :hijack_netrw true
   :hijack_cursor true
   :open_on_tab true
   :update_cwd true
   :git {:enable true :ignore true}
   :hijack_directories {:enable true :auto_open true}
   :renderer {:indent_markers {:enable false}}})

(setup :gitsigns
  {:signcolumn false
   :numhl true})

; (local ts (require :nvim-treesitter))
; (local tsq (require :nvim-treesitter.query))
; (local tsp (require :nvim-treesitter.parsers))

(setup :nvim-treesitter.configs
  {:ensure_installed [ "c" "cpp" "fennel" "rust" "racket"]
   :ensure_maintained "maintained"
   :sync_install false
   ;; :indent {:enable true}
   :incremental_selection {:enable true}
   :highlight { :enable true
                :additional_vim_regex_highlighting false
                :indent {:enable true}}})


(setup :neogit 
       {:disable_signs false
        :disable_hint true
        ;; :graph_style "unicode"
        :disable_context_highlighting true
        :disable_builtin_notifications true
        :status {:recent_commit_count 30}
        :signs {:section ["" ""]
                :item ["" ""]
                :hunk ["" ""]}
        :integrations {:diffview true}
        :sections {:recent {:folded false}}
        :mappings {:popup {:p false}}}
      (fn [neogit]
        ;; map \g to open neogit
        (keys.map "<leader>g" "Open neogit" neogit.open)))


(setup :nvim-highlight-colors {})

(setup :Comment {})

(setup :notify {}
       (fn [notify] (set vim.notify notify)))

(keys.map "<C-S-Left>"  "Prev Tab" ":tabprev<CR>")
(keys.map "<C-S-Right>" "Next Tab" ":tabnext<CR>")

(vim.cmd "syntax enable")
(vim.cmd "set shell=fish")

(vim.cmd "let $FZF_DEFAULT_OPTS = '--reverse'")
(vim.cmd "autocmd TermOpen * setlocal nonumber norelativenumber")

(vim.cmd "au BufRead,BufNewFile *.y set ft=haskell")
(vim.cmd "au BufRead,BufNewFile *.x set ft=haskell")
(vim.cmd "au BufRead,BufNewFile *.fz set syntax=futz")
(vim.cmd "au BufRead,BufNewFile *.fz set filetype=futz")
(vim.cmd "autocmd FileType futz setlocal commentstring=--%s")


; (local colors (require :highlight))
; (local darktheme colors.material-black)
; (local lighttheme colors.one-light)
;
; (state.register :colorscheme colors.select-colorscheme)
; (state.default :colorscheme darktheme)
;
; (keys.map "<leader>1" "Select Dark Theme"
;   (fn [] (state.set-val :colorscheme darktheme))
;   ;; (fn [] (colors.select-colorscheme darktheme))
;   {})
;
; (keys.map "<leader>2" "Select Light Theme"
;   (fn [] (state.set-val :colorscheme lighttheme))
;   ;; (fn [] (colors.select-colorscheme lighttheme))
;   {})
;
;
; (keys.map "<leader>3" "Zen mode"
;   (fn [] (vim.cmd ":ZenMode"))
;   {})
;
; (colors.select-colorscheme darktheme)

(setup :catppuccin
       {:transparent_background false})

(vim.cmd "colorscheme catppuccin")

; (vim.cmd "colorscheme oxocarbon")
; (vim.api.nvim_set_hl 0 "Normal" {:bg "none"})
; (vim.api.nvim_set_hl 0 "NormalFloat" {:bg "none"})



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



(keys.map "<C-n>" "Focus on the tree view" ":NvimTreeToggle<CR>" {:mode "n"})
(keys.map "<C-S-n>" "Focus on the tree view" ":NvimTreeFocus<CR>" {:mode "n"})
(keys.map "<M-n>" "Close the tree view" ":NvimTreeClose<CR>" {:mode "n"})
(keys.map "?" "Display keymaps" ":WhichKey<CR>" {:mode "n"})
(keys.map "<C-f>" "Display git files" ":GFiles<CR>" {:mode "n"})
(keys.map "<C-p>" "Display all files" ":Files<CR>" {:mode "n"})
(keys.map "<leader>ca" "Open display code actions" ":CodeActionMenu" {:mode "n"})
(keys.map "<leader>f" "Clang Format" ":ClangFormat<CR>" {:silent true :mode "n"})
(keys.map "<M-f>" "Search" ":Rg<CR>" {:mode "n"})
; Some sane split commands
(keys.map "<C-_>" "Horizontal Split" ":sp<CR>" {:mode "n"})
(keys.map "<C-\\>" "Vertical Split" ":vsp<CR>" {:mode "n"})
(keys.map "<C-q>" "Close the current split" ":q<CR>" {:mode "n"})
(keys.map "<" "Dedent" "<gv" {:mode "v"})
(keys.map ">" "Indent" ">gv" {:mode "v"})
(keys.map "qq" "exit" ":q<CR>")
(keys.map "<space>" "Select the word under the cursor" "<ESC>viw")
(keys.map "<leader>P" "Run PackerSync" ":PackerSync"
     {:silent true
      :mode "n"})

;; Unmap recording... It's annoying if you don't use it
(vim.cmd "nnoremap <silent> Q q")
(vim.cmd "nnoremap <silent> q <Nop>")

; (map "<C-j>" "scroll down" "<ScrollWheelDown>" {:mode "n"})
; (map "<C-k>" "scroll up" "<ScrollWheelUp>" {:mode "n"})

(keys.map "<C-j>" "scroll down" "5j" {:mode "n"})
(keys.map "<C-k>" "scroll up" "5k" {:mode "n"})


;; (state.write {:foo "hello"})

(state.start)
