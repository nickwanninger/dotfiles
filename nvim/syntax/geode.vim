" Vim syntax file
" Language: geode

if exists("b:current_syntax")
	finish
endif


syn keyword geodeKeywords is include if else for match while
syn keyword geodeKeywords return func
syn keyword geodeKeywords int float byte string bool
syn keyword geodeKeywords let

syn match geodeNum "\<\d\+\>[df]?"
syn match geodeNum "\v<[0-9_]+>"
syn match geodeNum "\v<.?[0-9_]+[df]?>"
syn match geodeNum "\<0[bB][01]\+\>"
syn match geodeNum "\<0[xX]\x\+\>"
syn match geodeNum "\<0[oO]\o\+\>"

syn match geodeComment /#.*/

syn region geodeString start=/"/ skip=/\\"/ end=/"/

syn match geodeCharacter /'.'/

highlight default link geodeCharacter   Character
highlight default link geodeKeywords    Keyword
highlight default link geodeNum         Number
highlight default link geodeString      String
highlight default link geodeComment     Comment



