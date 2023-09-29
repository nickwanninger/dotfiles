-- This file is a relatively simple bootstrapping script for my neovim configuration
-- It's main job is to setup a bunch of sane defaults, make sure packer is installed,
-- the plugins are `used`, and the initial sync is performed.
-- As part of that sync, it will install "tangerine", which allows the rest of the
-- configuration to be made in "fennel", which is a lisp

vim.cmd([[
filetype off
set nocompatible
set noshowmode
set laststatus=0
set winminheight=0
set splitbelow
set splitright
set noeol
set tabstop=2
set shiftwidth=2
set expandtab
set nofoldenable     "don't fold by default
set foldmethod=indent   " fold on indentations
set foldnestmax=10   "only fold up to 10 levels
set foldlevel=1     " only show me first fold level
let g:indentLine_enabled = 1
set visualbell t_vb= " disable visual bell
set ttyfast  " we have a fast terminal
set lazyredraw
set backspace=indent,eol,start
set clipboard=unnamedplus
set mouse=a
set pastetoggle=<F2>
set sidescroll=10
set matchpairs+=<:>
set incsearch
set ignorecase
set smartcase
set showmatch
set smartindent
set noswapfile
set nobackup
set nowritebackup
set undofile
set undodir=~/.tmp//,/tmp//
set hidden
set shell=/bin/sh
set encoding=utf-8 " Necessary to show Unicode glyphs
set termguicolors     " enable true colors support
filetype plugin indent on    " required

let g:indentLine_fileTypeExclude=['help']
let g:indentLine_bufNameExclude=['NERD_tree.*']

hi CocFloat ctermbg=238 ctermfg=15
hi CocFloating ctermbg=238 guibg=238 ctermfg=15
hi Pmenu ctermbg=238 guibg=238 ctermfg=15

au BufRead,BufNewFile *.hbs set filetype=html


" augroup ScreenEdgeScroll
"   autocmd!
"   autocmd CursorMoved * if line('.') - &scrolloff <= line('w0') || line('.') + &scrolloff >= line('w$') | normal zz | endif
" augroup END

command WQ wq
command Wq wq
command W w
command Q q
nnoremap ; :


" TODO: move this to a which-key configuration in fennel
let g:tmux_navigator_no_mappings = 1 " disable builtin mappings, I think
nnoremap <silent> <M-Left> :TmuxNavigateLeft<cr>
nnoremap <silent> <M-Down> :TmuxNavigateDown<cr>
nnoremap <silent> <M-Up> :TmuxNavigateUp<cr>
nnoremap <silent> <M-Right> :TmuxNavigateRight<cr>
nnoremap <silent> <M-h> :TmuxNavigateLeft<cr>
nnoremap <silent> <M-j> :TmuxNavigateDown<cr>
nnoremap <silent> <M-k> :TmuxNavigateUp<cr>
nnoremap <silent> <M-l> :TmuxNavigateRight<cr>
"nnoremap <leader>m :FloatermNew make -j<CR>
nnoremap <leader>Q :qall<CR>

nnoremap <leader>g :FloatermNew --height=0.9 --width=0.9 lazygit<CR>
nnoremap <leader>m :FloatermNew --height=0.9 --width=0.9 emacs -e magit-status -e delete-other-windows<CR>


noremap <leader>f :ClangFormat<CR>
nnoremap <leader>P :PackerSync<CR>

imap <C-Bslash> λ
imap <C-w> <esc>dbi

" This keeps the current visual block selection active
" disable writing different filename with :w<filename>
:autocmd BufWritePre [:;]*
\   try | echoerr 'Forbidden file name: ' . expand('<afile>') | endtry

set nu

nmap <C-a> :TagbarToggle<CR>
map <C-c> :set nu!<CR>

autocmd filetype crontab setlocal nobackup nowritebackup

" au BufRead,BufNewFile *.nesl set filetype=nesl
" au BufRead,BufNewFile *.fun set filetype=sml
]])





-- This function ensures that our package manager, packer, has been installed.
local ensure_packer = function()
  local fn = vim.fn
  local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
  if fn.empty(fn.glob(install_path)) > 0 then

		print("Bootstrapping packer")
    fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
    vim.cmd [[packadd packer.nvim]]

		vim.cmd "redraw"
    return true
  end
  return false
end


local packer_bootstrap = ensure_packer()
local packer = require('packer')


packer.startup(function(use)
	-- packer manages itself
  use 'wbthomason/packer.nvim'

	-- Install tangerine
	use {
		'udayvir-singh/tangerine.nvim',
		-- When it's loaded, call this function to initialize tangerine and
		-- start the fennel configuration
		config = function()
			local nvim_dir = vim.fn.stdpath [[config]]
			require('tangerine').setup {
				-- Start by using the init.fnl file
				vimrc   = nvim_dir .. "/fnl/init.fnl",
				-- And set the 'include path' for fennel files to $RT/fnl
				source  = nvim_dir .. "/fnl",
				-- target  = nvim_dir .. "/lua",
				target = vim.fn.stdpath [[data]] .. "/tangerine",
				compiler = {
					verbose = false,
					-- if you want to compile before loading init.fnl (recommended)
					hooks = { "oninit", "onsave" },
					-- if you only want after VimEnter event has fired
					-- hooks = { "onenter" },
				}
			}
		end,
	}

	-- Merge Tmux stuff
	use 'christoomey/vim-tmux-navigator'

	-- A bunch of base16 themes
	use 'RRethy/nvim-base16'

	-- Fuzzy finder
	use {'junegunn/fzf', run='fzf#install()'}
	use 'junegunn/fzf.vim'
  use 'monkoose/fzf-hoogle.vim'

	-- tag bar on the right
	use 'preservim/tagbar'
	-- `gcc` key combo
	use 'numToStr/Comment.nvim'

	-- Language support
	use 'lluchs/vim-wren' -- wren (cause it's cool)
	use 'Shirk/vim-gas' -- Gnu Assembler
	use 'dag/vim-fish' -- Fish Shell

	-- some stuff that some people want
	use 'nvim-lua/plenary.nvim'

	-- Show git info next to the numbers
	use 'lewis6991/gitsigns.nvim'

	-- A nice tree on the left
	use 'kyazdani42/nvim-tree.lua'

	-- Pretty notifications
	use 'rcarriga/nvim-notify'

	-- Floating terminal that I use in a bunch of places
	use 'voldikss/vim-floaterm'

	-- Autocompletion & Autocomplete
	use 'ms-jpq/coq_nvim'
	use 'ms-jpq/coq.artifacts'
	use 'neovim/nvim-lspconfig'
	use 'ray-x/lsp_signature.nvim'
	use 'rhysd/vim-clang-format'
	use 'ErichDonGubler/lsp_lines.nvim'
	use 'nvim-treesitter/nvim-treesitter'

	-- use 'nvim-treesitter/playground)
	use 'weilbith/nvim-code-action-menu'
	use 'folke/which-key.nvim'
	use 'folke/zen-mode.nvim'

	-- Lisp stuff
	use 'gpanders/nvim-parinfer'
	use 'Olical/conjure'
	-- use 'rachitnigam/drracket.vim'
	-- use 'wlangstroth/vim-racket'
	-- Fennel syntax highlighting
	use 'bakpakin/fennel.vim'


  -- A nice library to watch if a file changes
  use 'rktjmp/fwatch.nvim'

  -- Automatically set up the configuration after cloning packer.nvim
  if packer_bootstrap then
  	packer.sync()
  end
end)
