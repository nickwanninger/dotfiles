(require-macros :hibiscus.packer)

(packer-setup)

;; Setup Packer
(packer
  ;; Merge tmux and vim navigation
  (use! :christoomey/vim-tmux-navigator)

  ;; Also manage tangerine
  (use! :udayvir-singh/tangerine.nvim)

  ;; Fennel syntax highlighting
  (use! :bakpakin/fennel.vim)

  ;; Colorschemes
  ; (use! :arzg/vim-colors-xcode)
  ; (use! :arcticicestudio/nord-vim)
  ; (use! :connorholyday/vim-snazzy)
  (use! :RRethy/nvim-base16)

  ;; Fuzzy finder
  (use! :junegunn/fzf :run (fn [] (comment vim.cmd "fzf#install()")))
  (use! :junegunn/fzf.vim)

  ;; The tag bar on the right
  (use! :preservim/tagbar)

  (comment use! :norcalli/nvim-colorizer.lua)

  (use! :numToStr/Comment.nvim)

  ;; Some additional language support packages
  (use! :lluchs/vim-wren)
  (use! :Shirk/vim-gas) ;; Gnu Assembler
  (use! :dag/vim-fish) ;; Fish Shell

  ;; Lua libraries
  (use! :nvim-lua/plenary.nvim)

  ;; Show git status in the gutter
  (use! :lewis6991/gitsigns.nvim)
  ;; Tree
  (use! :kyazdani42/nvim-tree.lua)

  ;; Notify
  (use! :rcarriga/nvim-notify)

  ;; Floating Terminal Thing
  (use! :voldikss/vim-floaterm)

  ;; language server support
  (use! :ms-jpq/coq_nvim)
  (use! :ms-jpq/coq.artifacts)
  (use! :neovim/nvim-lspconfig)
  (use! :ray-x/lsp_signature.nvim)
  (use! :rhysd/vim-clang-format)
  (use! :ErichDonGubler/lsp_lines.nvim)

  ;; Treesitter packages
  (use! :nvim-treesitter/nvim-treesitter :run ":TSUpdate")
  ;; Treesitter playground (view the S-Expression for the current code)
  (use! :nvim-treesitter/playground)
  ;; Code actions
  (use! :weilbith/nvim-code-action-menu)
  ;; Keybinds
  (use! :folke/which-key.nvim)
  ;; A decent zen-mode
  (use! :folke/zen-mode.nvim)
  
  (use! :gpanders/nvim-parinfer))

