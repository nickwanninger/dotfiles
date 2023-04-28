(require-macros :hibiscus.packer)

(packer-setup)

;; Setup Packer
(packer
  ;; Goyo writing mode
  (use! :junegunn/goyo.vim)

  ;; (use! :nvim-tree/nvim-web-devicons)
  ;; (use! :romgrk/barbar.nvim)

  (use! :junegunn/limelight.vim)

  ;; Merge tmux and vim navigation
  (use! :christoomey/vim-tmux-navigator)

  ;; Also manage tangerine
  (use! :udayvir-singh/tangerine.nvim)

  ;; Fennel syntax highlighting
  (use! :bakpakin/fennel.vim)

  ;; Colorschemes
  (use! :arzg/vim-colors-xcode)
  (use! :arcticicestudio/nord-vim)
  (use! :connorholyday/vim-snazzy)
  (use! :RRethy/nvim-base16)

  ;; Statusline replacement
  ;; (use! :vim-airline/vim-airline)
  ;; (use! :vim-airline/vim-airline-themes)
  
  ;; Fuzzy finder
  (use! :junegunn/fzf :run (fn [] (comment vim.cmd "fzf#install()")))
  (use! :junegunn/fzf.vim)

  (use! :preservim/tagbar)
  (use! :norcalli/nvim-colorizer.lua)

  (use! :numToStr/Comment.nvim)

  ;; Some additional language support packages
  (use! :lluchs/vim-wren)
  (use! :Shirk/vim-gas)
  (use! :dag/vim-fish)

  ;; Lua libraries
  (use! :nvim-lua/plenary.nvim)

  ;; Show git status in the gutter
  (use! :lewis6991/gitsigns.nvim)
  ;; Tree
  (use! :kyazdani42/nvim-tree.lua)

  ;; Github Copilot
  (comment use! :github/copilot.vim)

  ;; Notify
  (use! :rcarriga/nvim-notify)

  ;; language server support
  (use! :ms-jpq/coq_nvim)
  (use! :ms-jpq/coq.artifacts)
  (use! :neovim/nvim-lspconfig)
  (use! :ray-x/lsp_signature.nvim)
  (use! :rhysd/vim-clang-format)
  (use! :ErichDonGubler/lsp_lines.nvim)

  ;; Treesitter packages
  (use! :nvim-treesitter/nvim-treesitter :run ":TSUpdate")
  (use! :nvim-treesitter/playground)
  (use! :weilbith/nvim-code-action-menu)
  (use! :gpanders/nvim-parinfer)
  (use! :folke/which-key.nvim)

  (use! :folke/zen-mode.nvim)
  (comment use! :lukas-reineke/indent-blankline.nvim))

