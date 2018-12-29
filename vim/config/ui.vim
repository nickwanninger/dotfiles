filetype plugin indent on
set wrap linebreak nolist
set wrap
set textwidth=0 wrapmargin=0
syntax on

if (has("mac"))
	if (has("termguicolors"))
		" set termguicolors
	endif
endif


set background=dark

colorscheme onedark

" hi Comment cterm=italic,bold
" hi Commentguifg=#5f5faf ctermfg=NONE
hi Normal     ctermbg=NONE guibg=NONE
" hi CursorLine ctermbg=NONE guibg=NONE
" hi LineNr     ctermbg=NONE guibg=NONE
" hi SignColumn ctermbg=NONE guibg=NONE
" hi SpecialKey ctermbg=NONE guibg=NONE
hi Special ctermbg=NONE guibg=NONE
hi NonText ctermbg=NONE guibg=NONE

highlight ExtraWhitespace ctermbg=red guibg=#fabd2f
match ExtraWhitespace /\s\+\%#\@<!$/
