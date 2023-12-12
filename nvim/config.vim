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


" disable writing different filename with :w<filename>
:autocmd BufWritePre [:;]*
\   try | echoerr 'Forbidden file name: ' . expand('<afile>') | endtry

autocmd filetype crontab setlocal nobackup nowritebackup
