(local keys (require :core.keymap))
(local state (require :core.state))
(local {: setup} (require :core.setup))
(local io (require :core.io))

(require :lsp)
(require :statusline)


;; Setup a sane set of defaults
(set vim.o.nocompatible true)
(set vim.o.noshowmode true)
(set vim.o.laststatus 0)
(set vim.o.winminheight 0)
(set vim.o.splitbelow true)
(set vim.o.splitright true)
(set vim.o.noeol true)
(set vim.o.tabstop 2)
(set vim.o.shiftwidth 1)
(set vim.o.expandtab true)
(set vim.o.nofoldenable true) ; don't fold by default
(set vim.o.visualbell "t_vb=")
(set vim.o.ttyfast true)
(set vim.o.lazyredraw true)
(set vim.o.backspace "indent,eol,start")
(set vim.o.clipboard :unnamedplus)
(set vim.o.mouse :a)
(set vim.o.pastetoggle "<F2>")
(set vim.o.sidescroll 10)
(set vim.o.incsearch true)
(set vim.o.ignorecase true)
(set vim.o.smartcase true)
(set vim.o.showmatch true)
(set vim.o.smartindent true)
(set vim.o.noswapfile true)
(set vim.o.nobackup true)
(set vim.o.nowritebackup true)
(set vim.o.undofile true)
(set vim.o.undodir "~/.tmp//,/tmp//")
(set vim.o.hidden true)
(set vim.o.shell "/bin/sh")
(set vim.o.encoding "utf-8")  ; Necessary to show Unicode glyphs

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
  {;; :ensure_installed [ "c" "cpp" "fennel" "rust" "racket"]
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

 


(keys.map "<C-n>" "Focus on the tree view" ":NvimTreeToggle<CR>" {:mode "n"})
(keys.map "<C-S-n>" "Focus on the tree view" ":NvimTreeFocus<CR>" {:mode "n"})
(keys.map "<M-n>" "Close the tree view" ":NvimTreeClose<CR>" {:mode "n"})
(keys.map "?" "Display keymaps" ":WhichKey<CR>" {:mode "n"})

; (keys.map "<C-f>" "Display git files" ":GFiles<CR>" {:mode "n"})
; (keys.map "<C-p>" "Display all files" ":Files<CR>" {:mode "n"})

(keys.map "<M-f>" "Search" ":Telescope live_grep<CR>" {:mode "n"})
(keys.map "<C-f>" "Display git files" ":Telescope git_files<cr>" {:mode "n"})
(keys.map "<C-p>" "Display all files" ":Telescope find_files<CR>" {:mode "n"})

; Some sane split commands
(keys.map "<C-_>" "Horizontal Split" ":sp<CR>" {:mode "n"})
(keys.map "<C-\\>" "Vertical Split" ":vsp<CR>" {:mode "n"})
(keys.map "<C-q>" "Close the current split" ":q<CR>" {:mode "n"})
(keys.map "<" "Dedent" "<gv" {:mode "v"})
(keys.map ">" "Indent" ">gv" {:mode "v"})
(keys.map "qq" "exit" ":q<CR>")
(keys.map "<space>" "Select the word under the cursor" "<ESC>viw")

;; Unmap recording... It's annoying if you don't use it
(vim.cmd "nnoremap <silent> Q q")
(vim.cmd "nnoremap <silent> q <Nop>")

; (map "<C-j>" "scroll down" "<ScrollWheelDown>" {:mode "n"})
; (map "<C-k>" "scroll up" "<ScrollWheelUp>" {:mode "n"})

(keys.map "<C-j>" "scroll down" "5j" {:mode "n"})
(keys.map "<C-k>" "scroll up" "4k" {:mode "n"})

;; Map the Tmux movement commands
(keys.map "<M-Left>"  "Nav Left"  ":TmuxNavigateLeft<cr>"  {:silent true})
(keys.map "<M-Right>" "Nav Right" ":TmuxNavigateRight<cr>" {:silent true})
(keys.map "<M-Up>"    "Nav Up"    ":TmuxNavigateUp<cr>"    {:silent true})
(keys.map "<M-Down>"  "Nav Down"  ":TmuxNavigateDown<cr>"  {:silent true})


;; Leader bindings
(keys.map "<leader>l" "Open lazy" ":Lazy")
(keys.map "<leader>ca" "Open display code actions" ":CodeActionMenu" {:mode "n"})
(keys.map "<leader>f" "Clang Format" ":ClangFormat<CR>" {:silent true :mode "n"})
(keys.map "<leader>u" "Toggle Undotree" ":UndotreeToggle<cr>" {:silent true :mode "n"})

;; Misc
(keys.map "<C-Bslash>" "Place a lambda" "λ" {:mode "i"})


(set vim.wo.number true)
(keys.map "<C-c>" "Toggle line numbers"
    (fn [] (set vim.wo.number (not vim.wo.number))))

(state.start)
