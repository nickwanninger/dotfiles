(require-macros :hibiscus.packer)

(packer-setup)

;; Setup Packer
(packer
  ;; Let packer manage itself
  ;; (use! :wbthomason/packer.nvim)
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

  (use! :vim-airline/vim-airline)
  (use! :vim-airline/vim-airline-themes)
  (use! :rust-lang/rust.vim)
  (use! :junegunn/fzf :run (fn [] (comment vim.cmd "fzf#install()")))
  (use! :junegunn/fzf.vim)
  (use! :preservim/tagbar)
  (use! :lluchs/vim-wren)
  (use! :Shirk/vim-gas)
  (use! :dag/vim-fish)
  (use! :safv12/andromeda.vim)
  (use! :morhetz/gruvbox)
  (use! :EdenEast/nightfox.nvim)
  (use! :nvim-lua/plenary.nvim)
  (use! :lewis6991/gitsigns.nvim)
  ;; (use! :kyazdani42/nvim-web-devicons)
  (use! :kyazdani42/nvim-tree.lua)
  (use! :neovim/nvim-lspconfig)

  ;; load and have coq 
  (use! :ms-jpq/coq_nvim)
  (use! :ms-jpq/coq.artifacts)

  (use! :nvim-treesitter/nvim-treesitter :run ":TSUpdate")
  (use! :nvim-treesitter/playground)
  (use! :ray-x/lsp_signature.nvim)
  (use! :weilbith/nvim-code-action-menu)
  (use! :gpanders/nvim-parinfer)
  (use! :folke/which-key.nvim))

