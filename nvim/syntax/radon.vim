" Vim syntax file
" Language: radon

if exists("b:current_syntax")
	finish
endif


syn keyword radonKeywords is include if else for match while
syn keyword radonKeywords let def in
syn match   radonOperator "[-!#$%&\*\+/<=>\?@\\^|~:.]\+\|\<_\>"



syn keyword radonControl if then else cond of


syn match radonNum "\<\d\+\>[df]?"
syn match radonNum "\v<[0-9_]+>"
syn match radonNum "\v<.?[0-9_]+[df]?>"
syn match radonNum "\<0[bB][01]\+\>"
syn match radonNum "\<0[xX]\x\+\>"
syn match radonNum "\<0[oO]\o\+\>"

syn match radonComment /--.*/

syn region radonString start=/"/ skip=/\\"/ end=/"/

syn match radonCharacter /'.'/

highlight default link radonCharacter   Character
highlight default link radonKeywords    Keyword
highlight default link radonNum         Number
highlight default link radonString      String
highlight default link radonComment     Comment
highlight default link radonOperator    Operator
highlight default link radonControl     Structure




