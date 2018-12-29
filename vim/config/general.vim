" GENERAL
" a spot for general configuration and whatnot

" Don't try to be vi compatible
scriptencoding utf-8
filetype off
set nocompatible " Disable vi-compatibility
set noshowmode
set laststatus=0

set winminheight=0
noremap <C-A-left> <C-W>h<C-W>_
noremap <C-A-right> <C-W>l<C-W>_

set splitbelow
set splitright

set tabstop=2
set shiftwidth=2

set nofoldenable     "don't fold by default
set foldmethod=indent   " fold on indentations
set foldnestmax=10   "only fold up to 10 levels
set foldlevel=1     " only show me first fold level

" enable italics mode and other things
let &t_ZH="\e[3m"
let &t_ZR="\e[23m"


" disable folding, hopefully. Its the worse
set nofoldenable " folding sucks

" show unicode glyphs
set encoding=utf-8 " Necessary to show Unicode glyphs


" setup some filetype mappings
au BufRead,BufNewFile *.g set filetype=geode
au BufRead,BufNewFile *.wl set filetype=clojure
au BufRead,BufNewFile *.cdr set filetype=lisp
autocmd Filetype haskell setlocal expandtab tabstop=4 shiftwidth=4 softtabstop=4
autocmd Filetype lisp setlocal expandtab tabstop=2 shiftwidth=2 softtabstop=2


command WQ wq
command Wq wq
command W w
command Q q
nnoremap ; :


nnoremap <leader>d "_d
xnoremap <leader>d "_d
xnoremap <leader>p "_dP

map <C-\> <C-v>F\
map <space> <ESC>viw
nnoremap Q q
nnoremap q <Nop>
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
