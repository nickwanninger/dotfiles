
" Vim syntax file
" Language: tile

if exists("b:current_syntax")
	finish
endif


syn keyword tileKeywords match check produce transformer

syn match tileNum "\v<[0-9_]+>"
syn match tileComment /\/\/.*/
syn match tileCharacter /'.'/

syn match tileIdentifier "%[_[:alnum:]]*"
syn match tileLabel ":[_[:alnum:]]*"

hi def link tileCharacter   Character
hi def link tileKeywords    Keyword
" hi def link tileRegister    Special
" hi def link tileLabel       Function
hi def link tileNum         Number
hi def link tileComment     Comment
hi def link tileIdentifier  Identifier
