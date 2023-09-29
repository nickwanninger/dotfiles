
" Vim syntax file
" Language: futz

if exists("b:current_syntax")
	finish
endif


syn keyword futzKeywords let in do end case fn class def data to of super impl match on some
syn match futzKeywords "λ"

syn match   futzOperator "[-!#$%&\*\+/<=>\?@\\^|~:.]\+\|\<_\>"

syn match	futzType		"\<[A-Z][a-zA-Z0-9_']*\>"
syn match	futzType		"\<\'[a-zA-Z0-9_']*\>"


syn keyword futzControl if then else

syn match futzNum "\<\d\+\>[df]?"
syn match futzNum "\v<[0-9_]+>"
syn match futzNum "\v<.?[0-9_]+[df]?>"
syn match futzNum "\<0[bB][01]\+\>"
syn match futzNum "\<0[xX]\x\+\>"
syn match futzNum "\<0[oO]\o\+\>"


syn match futzCharacter /'.'/

syn region futzString start=/"/ skip=/\\"/ end=/"/


syn match futzIdentifier "%[_[:alnum:]]*"

syn match futzComment "---*\([^-!#$%&\*\+./<=>\?@\\^|~].*\)\?$"
" syn match haskellLineComment "---*\([^-!#$%&\*\+./<=>\?@\\^|~].*\)\?$"


" Basic builtins
syn keyword futzBuiltin rem abs max min


hi def link futzOperator    Operator
hi def link futzType        Type

hi def link futzCharacter   Character
hi def link futzKeywords    Keyword
hi def link futzRegister    Special
hi def link futzNum         Number
hi def link futzComment     Comment
hi def link futzIdentifier  Identifier
hi def link futzControl     Structure
hi def link futzString      String

hi def link futzBuiltin     Function
