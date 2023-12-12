" This file just contains a bunch of default vim configurations that
" I couldn't quite figure out how to do using the lua API. :)

set termguicolors     " enable true colors support
filetype off
filetype plugin indent on    " required

" I don't like having to lift my finger from shift sometimes :)
command WQ wq
command Wq wq
command W w
command Q q
nnoremap ; :

let g:tmux_navigator_no_mappings = 1 " disable builtin mappings, I think


" disable writing different filename with :w<filename>
:autocmd BufWritePre [:;]* try | echoerr 'Forbidden file name: ' . expand('<afile>') | endtry

autocmd filetype crontab setlocal nobackup nowritebackup


" press <Tab> to expand or jump in a snippet. These can also be mapped separately
" via <Plug>luasnip-expand-snippet and <Plug>luasnip-jump-next.
imap <silent><expr> <Tab> luasnip#expand_or_jumpable() ? '<Plug>luasnip-expand-or-jump' : '<Tab>' 
" -1 for jumping backwards.
inoremap <silent> <S-Tab> <cmd>lua require'luasnip'.jump(-1)<Cr>

snoremap <silent> <Tab> <cmd>lua require('luasnip').jump(1)<Cr>
snoremap <silent> <S-Tab> <cmd>lua require('luasnip').jump(-1)<Cr>

" For changing choices in choiceNodes (not strictly necessary for a basic setup).
imap <silent><expr> <C-E> luasnip#choice_active() ? '<Plug>luasnip-next-choice' : '<C-E>'
smap <silent><expr> <C-E> luasnip#choice_active() ? '<Plug>luasnip-next-choice' : '<C-E>'

