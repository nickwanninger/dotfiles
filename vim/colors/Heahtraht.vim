" Vim color file
" Converted from Textmate theme Heáhtraht using Coloration v0.4.0 (http://github.com/sickill/coloration)

set background=dark
highlight clear

if exists("syntax_on")
  syntax reset
endif

let g:colors_name = "Heahtraht"

hi Cursor ctermfg=233 ctermbg=79 cterm=NONE guifg=#111111 guibg=#ffffff gui=NONE
hi Visual ctermfg=NONE ctermbg=236 cterm=NONE guifg=NONE guibg=#333333 gui=NONE
hi CursorLine ctermfg=NONE ctermbg=235 cterm=NONE guifg=NONE guibg=#292929 gui=NONE
hi CursorColumn ctermfg=NONE ctermbg=235 cterm=NONE guifg=NONE guibg=#292929 gui=NONE
hi ColorColumn ctermfg=NONE ctermbg=235 cterm=NONE guifg=NONE guibg=#292929 gui=NONE
hi LineNr ctermfg=245 ctermbg=235 cterm=NONE guifg=#888888 guibg=#292929 gui=NONE
hi VertSplit ctermfg=240 ctermbg=240 cterm=NONE guifg=#565656 guibg=#565656 gui=NONE
hi MatchParen ctermfg=243 ctermbg=NONE cterm=underline guifg=#7b7b7b guibg=NONE gui=underline
hi StatusLine ctermfg=15 ctermbg=240 cterm=bold guifg=#ffffff guibg=#565656 gui=bold
hi StatusLineNC ctermfg=15 ctermbg=240 cterm=NONE guifg=#ffffff guibg=#565656 gui=NONE
hi Pmenu ctermfg=NONE ctermbg=NONE cterm=NONE guifg=NONE guibg=NONE gui=NONE
hi PmenuSel ctermfg=NONE ctermbg=236 cterm=NONE guifg=NONE guibg=#333333 gui=NONE
hi IncSearch ctermfg=0 ctermbg=11 cterm=NONE guifg=#111111 guibg=#969696 gui=NONE
hi Search ctermfg=0 ctermbg=84 cterm=underline guifg=NONE guibg=NONE gui=underline
hi Directory ctermfg=240 ctermbg=NONE cterm=NONE guifg=#555555 guibg=NONE gui=NONE
hi Folded ctermfg=239 ctermbg=233 cterm=NONE guifg=#535353 guibg=#111111 gui=NONE

hi Normal ctermfg=15 ctermbg=233 cterm=NONE guifg=#ffffff guibg=#111111 gui=NONE
hi Boolean ctermfg=79 ctermbg=NONE cterm=NONE guifg=#49cdac guibg=NONE gui=NONE
hi Character ctermfg=240 ctermbg=NONE cterm=NONE guifg=#555555 guibg=NONE gui=NONE
hi Comment ctermfg=239 ctermbg=NONE cterm=NONE guifg=#535353 guibg=NONE gui=NONE
hi Conditional ctermfg=243 ctermbg=NONE cterm=NONE guifg=#7b7b7b guibg=NONE gui=NONE
hi Constant ctermfg=NONE ctermbg=NONE cterm=NONE guifg=NONE guibg=NONE gui=NONE
hi Define ctermfg=79 ctermbg=NONE cterm=NONE guifg=#7b7b7b guibg=NONE gui=NONE
hi DiffAdd ctermfg=15 ctermbg=64 cterm=bold guifg=#ffffff guibg=#427f08 gui=bold
hi DiffDelete ctermfg=88 ctermbg=NONE cterm=NONE guifg=#870303 guibg=NONE gui=NONE
hi DiffChange ctermfg=15 ctermbg=17 cterm=NONE guifg=#ffffff guibg=#182e4c gui=NONE
hi DiffText ctermfg=15 ctermbg=24 cterm=bold guifg=#ffffff guibg=#204a87 gui=bold
hi ErrorMsg ctermfg=167 ctermbg=NONE cterm=bold guifg=#cd4a49 guibg=NONE gui=bold
hi WarningMsg ctermfg=167 ctermbg=NONE cterm=bold guifg=#cd4a49 guibg=NONE gui=bold
hi Float ctermfg=240 ctermbg=NONE cterm=NONE guifg=#555555 guibg=NONE gui=NONE
hi Function ctermfg=245 ctermbg=NONE cterm=NONE guifg=#888888 guibg=NONE gui=NONE
hi Identifier ctermfg=15 ctermbg=NONE cterm=bold guifg=#49cdac guibg=NONE gui=bold
hi Keyword ctermfg=79 ctermbg=NONE cterm=NONE guifg=#49cdac guibg=NONE gui=NONE
hi Label ctermfg=246 ctermbg=NONE cterm=NONE guifg=#969696 guibg=NONE gui=NONE
hi NonText ctermfg=59 ctermbg=234 cterm=NONE guifg=#3b3a32 guibg=#1d1d1d gui=NONE
hi Number ctermfg=81 ctermbg=NONE cterm=NONE guifg=#5fd7ff guibg=NONE gui=NONE
hi Operator ctermfg=243 ctermbg=NONE cterm=NONE guifg=#7b7b7b guibg=NONE gui=NONE
hi PreProc ctermfg=243 ctermbg=NONE cterm=NONE guifg=#7b7b7b guibg=NONE gui=NONE
hi Special ctermfg=15 ctermbg=NONE cterm=NONE guifg=#969696 guibg=NONE gui=NONE
hi SpecialKey ctermfg=59 ctermbg=235 cterm=NONE guifg=#49cdac guibg=#292929 gui=NONE
hi Statement ctermfg=243 ctermbg=NONE cterm=NONE guifg=#49cdac guibg=NONE gui=NONE
hi StorageClass ctermfg=15 ctermbg=NONE cterm=bold guifg=#ffffff guibg=NONE gui=bold
hi String ctermfg=79 ctermbg=NONE cterm=NONE guifg=#49cdac guibg=NONE gui=NONE
hi Tag ctermfg=248 ctermbg=NONE cterm=NONE guifg=#aaaaaa guibg=NONE gui=NONE
hi Title ctermfg=15 ctermbg=NONE cterm=bold guifg=#ffffff guibg=NONE gui=bold
hi Todo ctermfg=239 ctermbg=NONE cterm=inverse,bold guifg=#535353 guibg=NONE gui=inverse,bold
hi Type ctermfg=81 ctermbg=NONE cterm=NONE guifg=#5fd7ff guibg=NONE gui=NONE
hi Underlined ctermfg=NONE ctermbg=NONE cterm=underline guifg=NONE guibg=NONE gui=underline
hi rubyClass ctermfg=243 ctermbg=NONE cterm=NONE guifg=#7b7b7b guibg=NONE gui=NONE
hi rubyFunction ctermfg=245 ctermbg=NONE cterm=NONE guifg=#888888 guibg=NONE gui=NONE
hi rubyInterpolationDelimiter ctermfg=NONE ctermbg=NONE cterm=NONE guifg=NONE guibg=NONE gui=NONE
hi rubySymbol ctermfg=240 ctermbg=NONE cterm=NONE guifg=#555555 guibg=NONE gui=NONE
hi rubyConstant ctermfg=252 ctermbg=NONE cterm=NONE guifg=#cccccc guibg=NONE gui=italic
hi rubyStringDelimiter ctermfg=246 ctermbg=NONE cterm=NONE guifg=#969696 guibg=NONE gui=NONE
hi rubyBlockParameter ctermfg=253 ctermbg=NONE cterm=NONE guifg=#d8d8d8 guibg=NONE gui=italic
hi rubyInstanceVariable ctermfg=NONE ctermbg=NONE cterm=NONE guifg=NONE guibg=NONE gui=NONE
hi rubyInclude ctermfg=243 ctermbg=NONE cterm=NONE guifg=#7b7b7b guibg=NONE gui=NONE
hi rubyGlobalVariable ctermfg=NONE ctermbg=NONE cterm=NONE guifg=NONE guibg=NONE gui=NONE
hi rubyRegexp ctermfg=246 ctermbg=NONE cterm=NONE guifg=#969696 guibg=NONE gui=NONE
hi rubyRegexpDelimiter ctermfg=246 ctermbg=NONE cterm=NONE guifg=#969696 guibg=NONE gui=NONE
hi rubyEscape ctermfg=240 ctermbg=NONE cterm=NONE guifg=#555555 guibg=NONE gui=NONE
hi rubyControl ctermfg=243 ctermbg=NONE cterm=NONE guifg=#7b7b7b guibg=NONE gui=NONE
hi rubyClassVariable ctermfg=NONE ctermbg=NONE cterm=NONE guifg=NONE guibg=NONE gui=NONE
hi rubyOperator ctermfg=243 ctermbg=NONE cterm=NONE guifg=#7b7b7b guibg=NONE gui=NONE
hi rubyException ctermfg=243 ctermbg=NONE cterm=NONE guifg=#7b7b7b guibg=NONE gui=NONE
hi rubyPseudoVariable ctermfg=NONE ctermbg=NONE cterm=NONE guifg=NONE guibg=NONE gui=NONE
hi rubyRailsUserClass ctermfg=252 ctermbg=NONE cterm=NONE guifg=#cccccc guibg=NONE gui=italic
hi rubyRailsARAssociationMethod ctermfg=245 ctermbg=NONE cterm=NONE guifg=#888888 guibg=NONE gui=NONE
hi rubyRailsARMethod ctermfg=245 ctermbg=NONE cterm=NONE guifg=#888888 guibg=NONE gui=NONE
hi rubyRailsRenderMethod ctermfg=245 ctermbg=NONE cterm=NONE guifg=#888888 guibg=NONE gui=NONE
hi rubyRailsMethod ctermfg=245 ctermbg=NONE cterm=NONE guifg=#888888 guibg=NONE gui=NONE
hi erubyDelimiter ctermfg=240 ctermbg=NONE cterm=NONE guifg=#555555 guibg=NONE gui=NONE
hi erubyComment ctermfg=239 ctermbg=NONE cterm=NONE guifg=#535353 guibg=NONE gui=NONE
hi erubyRailsMethod ctermfg=245 ctermbg=NONE cterm=NONE guifg=#888888 guibg=NONE gui=NONE
hi htmlTag ctermfg=NONE ctermbg=NONE cterm=NONE guifg=NONE guibg=NONE gui=NONE
hi htmlEndTag ctermfg=NONE ctermbg=NONE cterm=NONE guifg=NONE guibg=NONE gui=NONE
hi htmlTagName ctermfg=NONE ctermbg=NONE cterm=NONE guifg=NONE guibg=NONE gui=NONE
hi htmlArg ctermfg=NONE ctermbg=NONE cterm=NONE guifg=NONE guibg=NONE gui=NONE
hi htmlSpecialChar ctermfg=240 ctermbg=NONE cterm=NONE guifg=#555555 guibg=NONE gui=NONE
hi javaScriptFunction ctermfg=15 ctermbg=NONE cterm=bold guifg=#ffffff guibg=NONE gui=bold
hi javaScriptRailsFunction ctermfg=245 ctermbg=NONE cterm=NONE guifg=#888888 guibg=NONE gui=NONE
hi javaScriptBraces ctermfg=NONE ctermbg=NONE cterm=NONE guifg=NONE guibg=NONE gui=NONE
hi yamlKey ctermfg=248 ctermbg=NONE cterm=NONE guifg=#aaaaaa guibg=NONE gui=NONE
hi yamlAnchor ctermfg=NONE ctermbg=NONE cterm=NONE guifg=NONE guibg=NONE gui=NONE
hi yamlAlias ctermfg=NONE ctermbg=NONE cterm=NONE guifg=NONE guibg=NONE gui=NONE
hi yamlDocumentHeader ctermfg=246 ctermbg=NONE cterm=NONE guifg=#969696 guibg=NONE gui=NONE
hi cssURL ctermfg=253 ctermbg=NONE cterm=NONE guifg=#d8d8d8 guibg=NONE gui=italic
hi cssFunctionName ctermfg=245 ctermbg=NONE cterm=NONE guifg=#888888 guibg=NONE gui=NONE
hi cssColor ctermfg=240 ctermbg=NONE cterm=NONE guifg=#555555 guibg=NONE gui=NONE
hi cssPseudoClassId ctermfg=240 ctermbg=NONE cterm=NONE guifg=#555555 guibg=NONE gui=NONE
hi cssClassName ctermfg=240 ctermbg=NONE cterm=NONE guifg=#555555 guibg=NONE gui=NONE
hi cssValueLength ctermfg=240 ctermbg=NONE cterm=NONE guifg=#555555 guibg=NONE gui=NONE
hi cssCommonAttr ctermfg=241 ctermbg=NONE cterm=NONE guifg=#666666 guibg=NONE gui=NONE
hi cssBraces ctermfg=240 ctermbg=NONE cterm=NONE guifg=#555555 guibg=NONE gui=NONE
