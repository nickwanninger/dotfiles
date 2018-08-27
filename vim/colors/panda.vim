" Name:   Panda vim colorscheme
" Author: Caleb taylor <caleb89taylor@gmail.com>
" URL:    http://github.com/ctaylo21/pandavim
" Description: Attempt at a simplistic dark colorcheme for gVim

set background=dark
highlight clear
if exists("syntax_on")
  syntax reset
endif
let g:colors_name = "panda"

highlight Normal guibg=#191919 guifg=#EEEEEE gui=NONE
highlight link Type Normal
highlight link StorageClass Normal

highlight Identifier guibg=NONE guifg=#C6B6EE gui=NONE
highlight LineNr guibg=NONE guifg=#C6B6EE gui=NONE
highlight link Constant Identifier
highlight link Function Identifier
highlight link Define Identifier

highlight Structure guibg=NONE guifg=#EEEEEE gui=NONE
highlight Comment guibg=NONE guifg=#BBBBBB gui=NONE
highlight TabLine guibg=NONE guifg=#BBBBBB gui=NONE
highlight TabFill guibg=NONE guifg=#BBBBBB gui=NONE
highlight TabLineSel guibg=NONE guifg=#BBBBBB gui=NONE

highlight String guibg=NONE guifg=#97CDFF gui=NONE
highlight link Boolean String

highlight Statement guibg=NONE guifg=#EEEEEE gui=NONE
highlight Delimiter guibg=NONE guifg=#EEEEEE gui=NONE
hi Conditional guibg=NONE guifg=#6AB36A gui=NONE

" Vim specific stuff
highlight Search guibg=#A36666 guifg=#EEEEEE gui=NONE
highlight Folded guibg=#333333 guifg=#6AB36A gui=NONE
highlight helpNote guibg=#A36666 guifg=#EEEEEE gui=NONE
highlight Visual guibg=#474747 guifg=NONE gui=NONE
highlight CursorLine guibg=#474747 guifg=NONE gui=NONE
highlight MatchParen guibg=#A36666 guifg=NONE gui=NONE
highlight VimHiTerm guibg=NONE guifg=#6AB36A gui=NONE
highlight VimGroup guibg=NONE guifg=#6AB36A gui=NONE
highlight helpOption guibg=NONE guifg=#6AB36A gui=NONE
highlight vimOption guibg=NONE guifg=#6AB36A gui=NONE
highlight vimHiAttrib guibg=NONE guifg=#97CDFF gui=NONE
highlight vimSynType guibg=NONE guifg=#97CDFF gui=NONE
highlight vimCommentTitle guibg=NONE guifg=#97CDFF gui=NONE


" Popup menu
highlight Pmenu guibg=#333333 guifg=NONE gui=NONE
highlight PmenuSel guibg=#C6B6EE guifg=#333333 gui=NONE

" PHP
hi link phpType Conditional
highlight phpRegion guibg=NONE guifg=#EEEEEE gui=NONE
highlight phpKeyword guibg=NONE guifg=#6AB36A gui=NONE
highlight link phpBoolean String

" HTML
highlight htmlString guibg=NONE guifg=#C6B6EE gui=NONE
highlight htmlArg guibg=NONE guifg=#EEEEEE gui=NONE
highlight htmlTag guibg=NONE guifg=#EEEEEE gui=NONE
highlight htmlSpecialChar guibg=NONE guifg=#A36666 gui=NONE

" NERDTree
highlight Title guibg=NONE guifg=#EEEEEE gui=NONE
highlight NERDTreeRo guibg=NONE guifg=#A36666 gui=Bold
highlight Directory guibg=NONE guifg=#C6B6EE gui=Bold

" BufExplorer
highlight bufExplorerCurBuf guibg=NONE guifg=#6AB36A gui=NONE
highlight bufExplorerHelp guibg=NONE guifg=#BBBBBB gui=NONE
highlight bufExplorerHidBuf guibg=NONE guifg=#BBBBBB gui=NONE
