" Vim color file
" Converted from Textmate theme Heáhtraht using Coloration v0.4.0 (http://github.com/sickill/coloration)

set background=dark
highlight clear

if exists("syntax_on")
  syntax reset
endif

let g:colors_name = "Heahtraht"


hi Normal ctermfg=15 ctermbg=233 cterm=NONE

hi Cursor ctermfg=0 ctermbg=15 cterm=NONE
hi Visual ctermfg=NONE ctermbg=234 cterm=NONE
hi CursorLine ctermfg=NONE ctermbg=NONE cterm=bold
hi CursorColumn ctermfg=NONE ctermbg=0 cterm=NONE
hi ColorColumn ctermfg=NONE ctermbg=0 cterm=NONE


hi LineNr ctermfg=235 ctermbg=233 cterm=NONE

hi VertSplit ctermfg=240 ctermbg=240 cterm=NONE
hi MatchParen ctermfg=243 ctermbg=NONE cterm=underline
hi StatusLine ctermfg=15 ctermbg=240 cterm=bold
hi StatusLineNC ctermfg=15 ctermbg=240 cterm=NONE
hi Pmenu ctermfg=NONE ctermbg=NONE cterm=NONE
hi PmenuSel ctermfg=NONE ctermbg=236 cterm=NONE
hi IncSearch ctermfg=NONE ctermbg=NONE cterm=inverse
hi Search ctermfg=NONE ctermbg=11 cterm=inverse
hi Directory ctermfg=240 ctermbg=NONE cterm=NONE

hi Folded ctermfg=239 ctermbg=233 cterm=NONE

hi Visual ctermfg=0 ctermbg=79 cterm=NONE


hi Boolean ctermfg=79 ctermbg=NONE cterm=NONE
hi Character ctermfg=240 ctermbg=NONE cterm=NONE
hi Comment ctermfg=239 ctermbg=NONE cterm=NONE
hi Conditional ctermfg=243 ctermbg=NONE cterm=NONE
hi Constant ctermfg=NONE ctermbg=NONE cterm=NONE
hi Define ctermfg=79 ctermbg=NONE cterm=NONE

" Diffs
hi DiffAdd ctermfg=15 ctermbg=64 cterm=bold
hi DiffDelete ctermfg=88 ctermbg=NONE cterm=NONE
hi DiffChange ctermfg=15 ctermbg=17 cterm=NONE
hi DiffText ctermfg=15 ctermbg=24 cterm=bold

" Msgs
hi ErrorMsg ctermfg=167 ctermbg=NONE cterm=bold
hi Todo ctermfg=239 ctermbg=NONE cterm=inverse,bold
hi WarningMsg ctermfg=167 ctermbg=NONE cterm=bold

hi Float ctermfg=15 ctermbg=233 cterm=NONE
hi Function ctermfg=245 ctermbg=NONE cterm=NONE
hi Identifier ctermfg=15 ctermbg=NONE cterm=NONE
hi Keyword ctermfg=79 ctermbg=NONE cterm=bold
hi Label ctermfg=246 ctermbg=NONE cterm=NONE
hi NonText ctermfg=59 ctermbg=234 cterm=NONE
hi Number ctermfg=81 ctermbg=NONE cterm=NONE
hi Operator ctermfg=243 ctermbg=NONE cterm=NONE
hi PreProc ctermfg=243 ctermbg=NONE cterm=NONE
hi Special ctermfg=15 ctermbg=NONE cterm=NONE
hi SpecialKey ctermfg=59 ctermbg=235 cterm=NONE
hi Statement ctermfg=79 ctermbg=NONE cterm=NONE
hi StorageClass ctermfg=15 ctermbg=NONE cterm=bold
hi String ctermfg=79 ctermbg=NONE cterm=NONE
hi Tag ctermfg=248 ctermbg=NONE cterm=NONE
hi Title ctermfg=15 ctermbg=NONE cterm=bold
hi Type ctermfg=81 ctermbg=NONE cterm=NONE
hi Underlined ctermfg=NONE ctermbg=NONE cterm=underline
