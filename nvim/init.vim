
"////////////////////////////////////////////////////////
"//                                                    //
"//           )         )          (       (           //
"//        ( /(      ( /(          )\ )  (  `          //
"//        )\()) (   )\()) (   (  (()/(  )\))(         //
"//       ((_)\  )\ ((_)\  )\  )\  /(_))((_)()\        //
"//        _((_)((_)  ((_)((_)((_)(_))  (_()((_)       //
"//       | \| || __|/ _ \\ \ / / |_ _| |  \/  |       //
"//       | .` || _|| (_) |\ V /   | |  | |\/| |       //
"//       |_|\_||___|\___/  \_/   |___| |_|  |_|       //
"//                                                    //
"////////////////////////////////////////////////////////

scriptencoding utf-8
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




"////////////////////////////////////////////////////////
let g:indentLine_fileTypeExclude=['help']
let g:indentLine_bufNameExclude=['NERD_tree.*']
"////////////////////////////////////////////////////////

hi CocFloat ctermbg=238 ctermfg=15
hi CocFloating ctermbg=238 guibg=238 ctermfg=15
hi Pmenu ctermbg=238 guibg=238 ctermfg=15

" enable italics mode and other things
let &t_ZH="\e[3m"
let &t_ZR="\e[23m"



au BufRead,BufNewFile *.hbs set filetype=html



command WQ wq
command Wq wq
command W w
command Q q
nnoremap ; :


let g:tmux_navigator_no_mappings = 1 " disable builtin mappings, I think
nnoremap <silent> <M-Left> :TmuxNavigateLeft<cr>
nnoremap <silent> <M-Down> :TmuxNavigateDown<cr>
nnoremap <silent> <M-Up> :TmuxNavigateUp<cr>
nnoremap <silent> <M-Right> :TmuxNavigateRight<cr>
nnoremap <silent> <M-h> :TmuxNavigateLeft<cr>
nnoremap <silent> <M-j> :TmuxNavigateDown<cr>
nnoremap <silent> <M-k> :TmuxNavigateUp<cr>
nnoremap <silent> <M-l> :TmuxNavigateRight<cr>
nnoremap <leader>m :make<CR>
nnoremap <leader>Q :qall<CR>

noremap <C-l> :tabprevious<CR>
noremap <C-h> :tabnext<CR>
noremap <leader>t :tabnew<CR>
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

" map <C-n> :NERDTreeToggle<CR>
map <C-q> :q<CR>
map <C-Q> :q!<CR>
map <leader>/ :let @/=''<cr>"

autocmd filetype crontab setlocal nobackup nowritebackup

" use ripgrep, as its better.
" if executable('rg')
"     set grepprg=rg\ --nogroup\ --nocolor\ --column
"     set grepformat=%f:%l:%c%m
" endif
