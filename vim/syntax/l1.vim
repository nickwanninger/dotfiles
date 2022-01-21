
" Vim syntax file
" Language: l1

if exists("b:current_syntax")
	finish
endif


syn keyword l1Keywords mem return cjump call goto
syn match l1Keywords "stack\-arg"
syn keyword l1Register rdi rsi rdx rcx r8 r9 rax rbx rbp r10 r11 r12 r13 r14 r15 rsp
syn match   l1Operator "[-!#$%&\*\+/<=>\?@\\^|~:.]\+\|\<_\>"


syn match l1Num "\v<[0-9_]+>"
syn match l1Comment /\/\/.*/
syn match l1Character /'.'/

syn match l1Identifier "%[_[:alnum:]]*"
syn match l1Label ":[_[:alnum:]]*"

hi def link l1Character   Character
hi def link l1Keywords    Keyword
hi def link l1Register    Special
hi def link l1Label       Function
hi def link l1Num         Number
hi def link l1Comment     Comment
hi def link l1Identifier  Identifier
hi def link radonOperator Operator
