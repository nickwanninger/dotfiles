
" Vim syntax file
" Language: nesl

if exists("b:current_syntax")
	finish
endif


syn keyword neslKeywords function datatype let int
syn match neslKeywords "stack\-arg"
"  syn keyword neslRegister rdi rsi rdx rcx r8 r9 rax rbx rbp r10 r11 r12 r13 r14 r15 rsp
syn match   neslOperator "[-!#$%&\*\+/<=>\?@\\^|~:.]\+\|\<_\>"

syn keyword	neslType		int long short char void bool Collection
syn keyword	neslType		signed unsigned float double


syn keyword neslControl if then else in

syn match neslNum "\<\d\+\>[df]?"
syn match neslNum "\v<[0-9_]+>"
syn match neslNum "\v<.?[0-9_]+[df]?>"
syn match neslNum "\<0[bB][01]\+\>"
syn match neslNum "\<0[xX]\x\+\>"
syn match neslNum "\<0[oO]\o\+\>"


syn match neslCharacter /'.'/

syn region neslString start=/"/ skip=/\\"/ end=/"/


syn match neslIdentifier "%[_[:alnum:]]*"

syn region neslComment start="%" end="%" contains=@neslComment fold keepend extend

" Basic math operations
syn keyword neslBuiltin rem abs max min
syn keyword neslBuiltin lshift rshift
syn keyword neslBuiltin sqrt isqrt ln log exp expt
syn keyword neslBuiltin sin cos tan asin acos atan
syn keyword neslBuiltin sinh cosh tanh
syn keyword neslBuiltin not or and xor nor nand
syn keyword neslBuiltin plusp minusp zerop oddp evenp
syn keyword neslBuiltin btoi code_char char_code
syn keyword neslBuiltin float ceil floor trunc round
syn keyword neslBuiltin rand rand_seed
syn keyword neslBuiltin pi max_int min_int


" Basic sequence operations
syn keyword neslBuiltin dist zip plus_scan min_scan max_scan or_scan and_scan
syn keyword neslBuiltin sum max_val min_val any all count max_index min_index
syn keyword neslBuiltin read write permute rotate reverse drop take
syn keyword neslBuiltin odd_elts even_elts interleave subseq
syn keyword neslBuiltin flatten partition bottop
" Other Functions
syn keyword neslBuiltin sort rank collect int_collect kth_smallest find
syn keyword neslBuiltin search_for_subseqs remove_duplicates mark_duplicates
syn keyword neslBuiltin union intersection name
syn keyword neslBuiltin transpose eql hash select identity

syn keyword neslBuiltin linify wordify 
syn keyword neslBuiltin lowercase uppercase string_eql 
syn keyword neslBuiltin parse_int parse_float
syn keyword neslBuiltin print_char print_string
syn keyword neslBuiltin write_object_to_file  write_string_to_file
syn keyword neslBuiltin append_string_to_file
syn keyword neslBuiltin read_object_from_file read_string_from_file
syn keyword neslBuiltin read_int_seq_from_file read_float_seq_from_file
syn keyword neslBuiltin open_in_file open_out_file close_file 
syn keyword neslBuiltin write_char write_string
syn keyword neslBuiltin read_char read_string read_line read_word 
syn keyword neslBuiltin open_check write_check read_check close_check
syn keyword neslBuiltin nullstr stdin stdout stderr
syn keyword neslBuiltin display
syn keyword neslBuiltin w_make_window w_kill_window
syn keyword neslBuiltin w_add_box w_add_text_box w_add_button w_add_button_stack
syn keyword neslBuiltin w_add_text
syn keyword neslBuiltin w_get_named_box w_reset_box_size w_clear_box
syn keyword neslBuiltin w_bounds_from_box w_box_scale w_bounding_box
syn keyword neslBuiltin w_draw_point w_draw_big_point w_draw_points 
syn keyword neslBuiltin w_draw_segments w_draw_string w_draw_rectangle 
syn keyword neslBuiltin w_shade_rectangle w_shade_polygon
syn keyword neslBuiltin w_write_text_centered w_write_text_left w_write_paragraph
syn keyword neslBuiltin w_get_input w_get_input_noblock w_get_button_input w_get_zoom_box
syn keyword neslBuiltin shell_command get_environment_variable spawn
syn keyword neslBuiltin time


hi def link neslOperator    Operator
hi def link neslType        Type

hi def link neslCharacter   Character
hi def link neslKeywords    Keyword
hi def link neslRegister    Special
hi def link neslNum         Number
hi def link neslComment     Comment
hi def link neslIdentifier  Identifier
hi def link neslControl     Structure
hi def link neslString      String

hi def link neslBuiltin     Function
