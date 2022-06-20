
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


" Most important thing first -- Colors
syntax enable



" let g:airline_theme='deus'

" let g:darktheme="nordfox"
let g:darktheme="xcodedark"
let g:lighttheme="xcodelight"


function ToggleColors()
		let s:is_dark=(&background == 'dark')
		syntax reset
    if s:is_dark
				set bg=light
				execute "colorscheme " . g:lighttheme
    else
				set bg=dark
				execute "colorscheme " . g:darktheme
				" hi Normal  ctermbg=NONE guibg=NONE
    endif

		hi clear SignColumn
endfunction

let $FZF_DEFAULT_OPTS="--reverse"
let g:asmsyntax = 'gas'



"////////////////////////////////////////////////////////
let g:indent_guides_enable_on_vim_startup = 1
let g:indent_guides_auto_colors = 0
"////////////////////////////////////////////////////////


noremap <C-l> :tabprevious<CR>
noremap <C-h> :tabnext<CR>
noremap <leader>t :tabnew<CR>


"////////////////////////////////////////////////////////
let g:tmux_navigator_no_mappings = 1 " disable builtin mappings, I think
nnoremap <silent> <M-Left> :TmuxNavigateLeft<cr>
nnoremap <silent> <M-Down> :TmuxNavigateDown<cr>
nnoremap <silent> <M-Up> :TmuxNavigateUp<cr>
nnoremap <silent> <M-Right> :TmuxNavigateRight<cr>
nnoremap <silent> <M-h> :TmuxNavigateLeft<cr>
nnoremap <silent> <M-j> :TmuxNavigateDown<cr>
nnoremap <silent> <M-k> :TmuxNavigateUp<cr>
nnoremap <silent> <M-l> :TmuxNavigateRight<cr>
nnoremap <silent> <C-M--> :sp<cr>
nnoremap <silent> <C-M-\> :vsp<cr>
nnoremap <C-p> :GFiles<CR>
nnoremap <C-f> :Files<CR>


nnoremap <C-n> :NvimTreeToggle<CR>
nnoremap <leader>ca :CodeActionMenu<CR>


"////////////////////////////////////////////////////////
let g:indentLine_fileTypeExclude=['help']
let g:indentLine_bufNameExclude=['NERD_tree.*']
"////////////////////////////////////////////////////////


"////////////////////////////////////////////////////////
filetype plugin indent on

hi CocFloat ctermbg=238 ctermfg=15
hi CocFloating ctermbg=238 guibg=238 ctermfg=15
hi Pmenu ctermbg=238 guibg=238 ctermfg=15

let g:gitgutter_max_signs = 500

let g:gitgutter_override_sign_column_highlight = 0


" highlight ExtraWhitespace ctermbg=red guibg=red
" match ExtraWhitespace /\s\+\%#\@<!$/
"////////////////////////////////////////////////////////

" enable italics mode and other things
let &t_ZH="\e[3m"
let &t_ZR="\e[23m"



au BufRead,BufNewFile *.hbs set filetype=html



command WQ wq
command Wq wq
command W w
command Q q
nnoremap ; :


nnoremap <leader>d "_d
xnoremap <leader>d "_d
xnoremap <leader>p "_dP


map <space> <ESC>viw
nnoremap Q q
nnoremap q <Nop>
nnoremap <leader>m :make<CR>



imap <C-Bslash> λ
imap <C-w> <esc>dbi


" This keeps the current visual block selection active
" after changing indent with '<' or '>'. Usually the
" visual block selection is lost after you shift it,
" which is incredibly annoying.
vmap > >gv
vmap < <gv

" disable writing different filename with :w<filename>
:autocmd BufWritePre [:;]*
\   try | echoerr 'Forbidden file name: ' . expand('<afile>') | endtry



" Change the scroll wheel from an actual scroll
" to just moving up and down with arrowkeys
map <ScrollWheelUp> <Up>
map <ScrollWheelDown> <Down>
map <ScrollWheelRight> <Right>
map <ScrollWheelLeft> <Left>


map <S-Up> <Up>
set nu

nmap <C-a> :TagbarToggle<CR>

map <C-c> :set nu!<CR>
" map <C-n> :NERDTreeToggle<CR>
map <C-q> :q<CR>
map <C-Q> :q!<CR>
map <leader>/ :let @/=''<cr>"


" nice control-s meme
noremap <C-S> :w<CR>
vnoremap <C-S> <C-C>:update<CR>
inoremap <C-S> <C-O>:update<CR>
inoremap <C-_> /*  */<Left><Left><Left>


autocmd filetype crontab setlocal nobackup nowritebackup


" setup some filetype mappings
au Filetype haskell setlocal expandtab tabstop=4 shiftwidth=4 softtabstop=4
au Filetype happy setlocal expandtab tabstop=4 shiftwidth=4 softtabstop=4
au Filetype lisp setlocal expandtab tabstop=2 shiftwidth=2 softtabstop=2
au Filetype clojure setlocal expandtab tabstop=2 shiftwidth=2 softtabstop=2

au BufRead,BufNewFile *.sig set filetype=sml
au BufRead,BufNewFile *.fun set filetype=sml

" use ripgrep, as its better.
if executable('rg')
    set grepprg=rg\ --nogroup\ --nocolor\ --column
    set grepformat=%f:%l:%c%m
endif
