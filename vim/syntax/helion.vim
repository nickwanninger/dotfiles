" Vim syntax file

" Prelude {{{1
if exists("b:current_syntax")
  finish
endif

" this file uses line continuations
let s:cpo_sav = &cpo
set cpo&vim

" Folding Config {{{1
setlocal foldmethod=syntax

let s:foldable_groups = split(
      \	  get(
      \	    b:,
      \	    'ruby_foldable_groups',
      \	    get(g:, 'ruby_foldable_groups', 'ALL')
      \	  )
      \	)

function! s:foldable(...) abort
  return 1
  if index(s:foldable_groups, 'ALL') > -1
    return 1
  endif

  for l:i in a:000
    if index(s:foldable_groups, l:i) > -1
      return 1
    endif
  endfor

  return 0
endfunction " }}}

syn cluster helionNotTop contains=@helionExtendedStringSpecial,@helionRegexpSpecial,@helionDeclaration,helionConditional,helionExceptional,helionMethodExceptional,helionTodo,helionModuleName,helionClassName,helionSymbolDelimiter

" Whitespace Errors {{{1
if exists("helion_space_errors")
  if !exists("helion_no_trail_space_error")
    syn match helionSpaceError display excludenl "\s\+$"
  endif
  if !exists("helion_no_tab_space_error")
    syn match helionSpaceError display " \+\t"me=e-1
  endif
endif

" Operators {{{1
if exists("helion_operators")
  syn match  helionOperator "[~!^|*/%+-]\|&\.\@!\|\%(type\s*\)\@<!<<\|<=>\|<=\|\%(<\|\<type\s\+\u\w*\s*\)\@<!<[^<]\@=\|===\|==\|=\~\|>>\|>=\|=\@1<!>\|\*\*\|\.\.\.\|\.\.\|::"
  syn match  helionOperator "->\|-=\|/=\|\*\*=\|\*=\|&&=\|&=\|&&\|||=\||=\|||\|%=\|+=\|!\~\|!="
  syn region helionBracketOperator matchgroup=helionOperator start="\%(\w[?!]\=\|[]})]\)\@2<=\[\s*" end="\s*]" contains=ALLBUT,@helionNotTop
endif

" Expression Substitution and Backslash Notation {{{1
syn match helionStringEscape "\\\\\|\\[abefnrstv]\|\\\o\{1,3}\|\\x\x\{1,2}"						    contained display
syn match helionStringEscape "\%(\\M-\\C-\|\\C-\\M-\|\\M-\\c\|\\c\\M-\|\\c\|\\C-\|\\M-\)\%(\\\o\{1,3}\|\\x\x\{1,2}\|\\\=\S\)" contained display
syn match helionQuoteEscape  "\\[\\']"											    contained display

syn region helionInterpolation	      matchgroup=helionInterpolationDelimiter start="#{" end="}" contained contains=ALLBUT,@helionNotTop
syn match  helionInterpolation	      "#\%(\$\|@@\=\)\w\+"    display contained contains=helionInterpolationDelimiter,helionInstanceVariable,helionClassVariable,helionGlobalVariable,helionPredefinedVariable
syn match  helionInterpolationDelimiter "#\ze\%(\$\|@@\=\)\w\+" display contained
syn match  helionInterpolation	      "#\$\%(-\w\|\W\)"       display contained contains=helionInterpolationDelimiter,helionPredefinedVariable,helionInvalidVariable
syn match  helionInterpolationDelimiter "#\ze\$\%(-\w\|\W\)"    display contained
syn region helionNoInterpolation	      start="\\#{" end="}"	      contained
syn match  helionNoInterpolation	      "\\#{"		      display contained
syn match  helionNoInterpolation	      "\\#\%(\$\|@@\=\)\w\+"  display contained
syn match  helionNoInterpolation	      "\\#\$\W"		      display contained

syn match helionDelimiterEscape	"\\[(<{\[)>}\]]" transparent display contained contains=NONE

syn region helionNestedParentheses    start="("  skip="\\\\\|\\)"  matchgroup=helionString end=")"	transparent contained
syn region helionNestedCurlyBraces    start="{"  skip="\\\\\|\\}"  matchgroup=helionString end="}"	transparent contained
syn region helionNestedAngleBrackets  start="<"  skip="\\\\\|\\>"  matchgroup=helionString end=">"	transparent contained
syn region helionNestedSquareBrackets start="\[" skip="\\\\\|\\\]" matchgroup=helionString end="\]"	transparent contained

" Regular Expression Metacharacters {{{1
" These are mostly Oniguruma ready
syn region helionRegexpComment	matchgroup=helionRegexpSpecial   start="(?#"								  skip="\\)"  end=")"  contained
syn region helionRegexpParens	matchgroup=helionRegexpSpecial   start="(\(?:\|?<\=[=!]\|?>\|?<[a-z_]\w*>\|?[imx]*-[imx]*:\=\|\%(?#\)\@!\)" skip="\\)"  end=")"  contained transparent contains=@helionRegexpSpecial
syn region helionRegexpBrackets	matchgroup=helionRegexpCharClass start="\[\^\="								  skip="\\\]" end="\]" contained transparent contains=helionStringEscape,helionRegexpEscape,helionRegexpCharClass oneline
syn match  helionRegexpCharClass	"\\[DdHhSsWw]"	       contained display
syn match  helionRegexpCharClass	"\[:\^\=\%(alnum\|alpha\|ascii\|blank\|cntrl\|digit\|graph\|lower\|print\|punct\|space\|upper\|xdigit\):\]" contained
syn match  helionRegexpEscape	"\\[].*?+^$|\\/(){}[]" contained
syn match  helionRegexpQuantifier	"[*?+][?+]\="	       contained display
syn match  helionRegexpQuantifier	"{\d\+\%(,\d*\)\=}?\=" contained display
syn match  helionRegexpAnchor	"[$^]\|\\[ABbGZz]"     contained display
syn match  helionRegexpDot	"\."		       contained display
syn match  helionRegexpSpecial	"|"		       contained display
syn match  helionRegexpSpecial	"\\[1-9]\d\=\d\@!"     contained display
syn match  helionRegexpSpecial	"\\k<\%([a-z_]\w*\|-\=\d\+\)\%([+-]\d\+\)\=>" contained display
syn match  helionRegexpSpecial	"\\k'\%([a-z_]\w*\|-\=\d\+\)\%([+-]\d\+\)\='" contained display
syn match  helionRegexpSpecial	"\\g<\%([a-z_]\w*\|-\=\d\+\)>" contained display
syn match  helionRegexpSpecial	"\\g'\%([a-z_]\w*\|-\=\d\+\)'" contained display

syn cluster helionStringSpecial	      contains=helionInterpolation,helionNoInterpolation,helionStringEscape
syn cluster helionExtendedStringSpecial contains=@helionStringSpecial,helionNestedParentheses,helionNestedCurlyBraces,helionNestedAngleBrackets,helionNestedSquareBrackets
syn cluster helionRegexpSpecial	      contains=helionInterpolation,helionNoInterpolation,helionStringEscape,helionRegexpSpecial,helionRegexpEscape,helionRegexpBrackets,helionRegexpCharClass,helionRegexpDot,helionRegexpQuantifier,helionRegexpAnchor,helionRegexpParens,helionRegexpComment

" Numbers and ASCII Codes {{{1
syn match helionASCIICode "\%(\w\|[]})\"'/]\)\@1<!\%(?\%(\\M-\\C-\|\\C-\\M-\|\\M-\\c\|\\c\\M-\|\\c\|\\C-\|\\M-\)\=\%(\\\o\{1,3}\|\\x\x\{1,2}\|\\\=\S\)\)"
syn match helionInteger	"\%(\%(\w\|[]})\"']\s*\)\@<!-\)\=\<0[xX]\x\+\%(_\x\+\)*r\=i\=\>"								display
syn match helionInteger	"\%(\%(\w\|[]})\"']\s*\)\@<!-\)\=\<\%(0[dD]\)\=\%(0\|[1-9]\d*\%(_\d\+\)*\)r\=i\=\>"						display
syn match helionInteger	"\%(\%(\w\|[]})\"']\s*\)\@<!-\)\=\<0[oO]\=\o\+\%(_\o\+\)*r\=i\=\>"								display
syn match helionInteger	"\%(\%(\w\|[]})\"']\s*\)\@<!-\)\=\<0[bB][01]\+\%(_[01]\+\)*r\=i\=\>"								display
syn match helionFloat	"\%(\%(\w\|[]})\"']\s*\)\@<!-\)\=\<\%(0\|[1-9]\d*\%(_\d\+\)*\)\.\d\+\%(_\d\+\)*r\=i\=\>"					display
syn match helionFloat	"\%(\%(\w\|[]})\"']\s*\)\@<!-\)\=\<\%(0\|[1-9]\d*\%(_\d\+\)*\)\%(\.\d\+\%(_\d\+\)*\)\=\%([eE][-+]\=\d\+\%(_\d\+\)*\)r\=i\=\>"	display

" Identifiers {{{1
syn match helionLocalVariableOrMethod "\<[_[:lower:]][_[:alnum:]]*[?!=]\=" contains=NONE display transparent
syn match helionBlockArgument	    "&[_[:lower:]][_[:alnum:]]"		 contains=NONE display transparent

syn match  helionClassName	"\%(\%(^\|[^.]\)\.\s*\)\@<!\<\u\%(\w\|[^\x00-\x7F]\)*\>\%(\s*(\)\@!" contained
syn match  helionModuleName	"\%(\%(^\|[^.]\)\.\s*\)\@<!\<\u\%(\w\|[^\x00-\x7F]\)*\>\%(\s*(\)\@!" contained
syn match  helionConstant		"\%(\%(^\|[^.]\)\.\s*\)\@<!\<\u\%(\w\|[^\x00-\x7F]\)*\>\%(\s*(\)\@!"
syn match  helionClassVariable	"@@\%(\h\|[^\x00-\x7F]\)\%(\w\|[^\x00-\x7F]\)*" display
syn match  helionInstanceVariable "@\%(\h\|[^\x00-\x7F]\)\%(\w\|[^\x00-\x7F]\)*"	display
syn match  helionGlobalVariable	"$\%(\%(\h\|[^\x00-\x7F]\)\%(\w\|[^\x00-\x7F]\)*\|-.\)"
syn match  helionSymbolDelimiter	":" contained
syn match  helionSymbol		"[]})\"':]\@1<!:\%(\^\|\~@\|\~\|<<\|<=>\|<=\|<\|===\|[=!]=\|[=!]\~\|!@\|!\|>>\|>=\|>\||\|-@\|-\|/\|\[]=\|\[]\|\*\*\|\*\|&\|%\|+@\|+\|`\)" contains=helionSymbolDelimiter
syn match  helionSymbol		"[]})\"':]\@1<!:\$\%(-.\|[`~<=>_,;:!?/.'"@$*\&+0]\)" contains=helionSymbolDelimiter
syn match  helionSymbol		"[]})\"':]\@1<!:\%(\$\|@@\=\)\=\%(\h\|[^\x00-\x7F]\)\%(\w\|[^\x00-\x7F]\)*" contains=helionSymbolDelimiter
syn match  helionSymbol		"[]})\"':]\@1<!:\%(\h\|[^\x00-\x7F]\)\%(\w\|[^\x00-\x7F]\)*\%([?!=]>\@!\)\=" contains=helionSymbolDelimiter

if s:foldable(':')
  syn region helionSymbol		matchgroup=helionSymbolDelimiter start="[]})\"':]\@1<!:'"  end="'"  skip="\\\\\|\\'"  contains=helionQuoteEscape fold
  syn region helionSymbol		matchgroup=helionSymbolDelimiter start="[]})\"':]\@1<!:\"" end="\"" skip="\\\\\|\\\"" contains=@helionStringSpecial fold
else
  syn region helionSymbol		matchgroup=helionSymbolDelimiter start="[]})\"':]\@1<!:'"  end="'"  skip="\\\\\|\\'"  contains=helionQuoteEscape
  syn region helionSymbol		matchgroup=helionSymbolDelimiter start="[]})\"':]\@1<!:\"" end="\"" skip="\\\\\|\\\"" contains=@helionStringSpecial
endif

syn match  helionCapitalizedMethod	"\%(\%(^\|[^.]\)\.\s*\)\@<!\<\u\%(\w\|[^\x00-\x7F]\)*\>\%(\s*(\)*\s*(\@="

syn match  helionBlockParameter	  "\%(\h\|[^\x00-\x7F]\)\%(\w\|[^\x00-\x7F]\)*" contained
syn region helionBlockParameterList start="\%(\%(\<do\>\|{\)\_s*\)\@32<=|" end="|" oneline display contains=helionBlockParameter

syn match helionInvalidVariable	 "$[^ A-Za-z_-]"
syn match helionPredefinedVariable #$[!$&"'*+,./0:;<=>?@\`~]#
syn match helionPredefinedVariable "$\d\+"										   display
syn match helionPredefinedVariable "$_\>"											   display
syn match helionPredefinedVariable "$-[0FIKadilpvw]\>"									   display
syn match helionPredefinedVariable "$\%(deferr\|defout\|stderr\|stdin\|stdout\)\>"					   display

" Normal Regular Expression {{{1
if s:foldable('/')
  syn region helionRegexp matchgroup=helionRegexpDelimiter start="\%(\%(^\|\<\%(and\|or\|while\|until\|unless\|if\|when\|not\|then\|else\)\|[;\~=!|&(,{[<>?:*+-]\)\s*\)\@<=/" end="/[iomxneus]*" skip="\\\\\|\\/" contains=@helionRegexpSpecial fold
  syn region helionRegexp matchgroup=helionRegexpDelimiter start="\%(\h\k*\s\+\)\@<=/\%([ \t=]\|$\)\@!" end="/[iomxneus]*" skip="\\\\\|\\/" contains=@helionRegexpSpecial fold
else
  syn region helionRegexp matchgroup=helionRegexpDelimiter start="\%(\%(^\|\<\%(and\|or\|while\|until\|unless\|if\|when\|not\|then\|else\)\|[;\~=!|&(,{[<>?:*+-]\)\s*\)\@<=/" end="/[iomxneus]*" skip="\\\\\|\\/" contains=@helionRegexpSpecial
  syn region helionRegexp matchgroup=helionRegexpDelimiter start="\%(\h\k*\s\+\)\@<=/\%([ \t=]\|$\)\@!" end="/[iomxneus]*" skip="\\\\\|\\/" contains=@helionRegexpSpecial
endif

" Generalized Regular Expression {{{1
if s:foldable('%')
  syn region helionRegexp matchgroup=helionRegexpDelimiter start="%r\z([~`!@#$%^&*_\-+=|\:;"',.?/]\)" end="\z1[iomxneus]*" skip="\\\\\|\\\z1" contains=@helionRegexpSpecial fold
  syn region helionRegexp matchgroup=helionRegexpDelimiter start="%r{"				end="}[iomxneus]*"   skip="\\\\\|\\}"	 contains=@helionRegexpSpecial fold
  syn region helionRegexp matchgroup=helionRegexpDelimiter start="%r<"				end=">[iomxneus]*"   skip="\\\\\|\\>"	 contains=@helionRegexpSpecial,helionNestedAngleBrackets,helionDelimiterEscape fold
  syn region helionRegexp matchgroup=helionRegexpDelimiter start="%r\["				end="\][iomxneus]*"  skip="\\\\\|\\\]"	 contains=@helionRegexpSpecial fold
  syn region helionRegexp matchgroup=helionRegexpDelimiter start="%r("				end=")[iomxneus]*"   skip="\\\\\|\\)"	 contains=@helionRegexpSpecial fold
  syn region helionRegexp matchgroup=helionRegexpDelimiter start="%r\z(\s\)"				end="\z1[iomxneus]*" skip="\\\\\|\\\z1" contains=@helionRegexpSpecial fold
else
  syn region helionRegexp matchgroup=helionRegexpDelimiter start="%r\z([~`!@#$%^&*_\-+=|\:;"',.?/]\)" end="\z1[iomxneus]*" skip="\\\\\|\\\z1" contains=@helionRegexpSpecial
  syn region helionRegexp matchgroup=helionRegexpDelimiter start="%r{"				end="}[iomxneus]*"   skip="\\\\\|\\}"	 contains=@helionRegexpSpecial
  syn region helionRegexp matchgroup=helionRegexpDelimiter start="%r<"				end=">[iomxneus]*"   skip="\\\\\|\\>"	 contains=@helionRegexpSpecial,helionNestedAngleBrackets,helionDelimiterEscape
  syn region helionRegexp matchgroup=helionRegexpDelimiter start="%r\["				end="\][iomxneus]*"  skip="\\\\\|\\\]"	 contains=@helionRegexpSpecial
  syn region helionRegexp matchgroup=helionRegexpDelimiter start="%r("				end=")[iomxneus]*"   skip="\\\\\|\\)"	 contains=@helionRegexpSpecial
  syn region helionRegexp matchgroup=helionRegexpDelimiter start="%r\z(\s\)"				end="\z1[iomxneus]*" skip="\\\\\|\\\z1" contains=@helionRegexpSpecial
endif

" Normal String {{{1
syn region helionString start="\"" end="\"" skip="\\\\\|\\\"" contains=@helionStringSpecial
syn region helionString start="'" end="'" skip="\\\\\|\\'"  contains=helionQuoteEscape




" Module, Class, Method and Alias Declarations {{{1
syn match  helionAliasDeclaration    "[^[:space:];#.()]\+" contained contains=helionSymbol,helionGlobalVariable,helionPredefinedVariable nextgroup=helionAliasDeclaration2 skipwhite
syn match  helionAliasDeclaration2   "[^[:space:];#.()]\+" contained contains=helionSymbol,helionGlobalVariable,helionPredefinedVariable
syn match  helionMethodDeclaration   "[^[:space:];#(]\+"	 contained contains=helionConstant,helionBoolean,helionPseudoVariable,helionInstanceVariable,helionClassVariable,helionGlobalVariable
syn match  helionClassDeclaration    "[^[:space:];#<]\+"	 contained contains=helionClassName,helionOperator
syn match  helionModuleDeclaration   "[^[:space:];#<]\+"	 contained contains=helionModuleName,helionOperator
syn match  helionMethodName "\<[_[:alpha:]][_[:alnum:]]*[?!=]\=[[:alnum:]_.:?!=]\@!" contained containedin=helionMethodDeclaration
syn match  helionMethodName "\%(\s\|^\)\@1<=[_[:alpha:]][_[:alnum:]]*[?!=]\=\%(\s\|$\)\@=" contained containedin=helionAliasDeclaration,helionAliasDeclaration2
syn match  helionMethodName "\%([[:space:].]\|^\)\@2<=\%(\[\]=\=\|\*\*\|[-+!~]@\=\|[*/%|&^~]\|<<\|>>\|[<>]=\=\|<=>\|===\|[=!]=\|[=!]\~\|!\|`\)\%([[:space:];#(]\|$\)\@=" contained containedin=helionAliasDeclaration,helionAliasDeclaration2,helionMethodDeclaration

syn cluster helionDeclaration contains=helionAliasDeclaration,helionAliasDeclaration2,helionMethodDeclaration,helionModuleDeclaration,helionClassDeclaration,helionMethodName,helionBlockParameter

" Keywords {{{1
" Note: the following keywords have already been defined:
" begin case class def do end for if module unless until while
syn match   helionControl	       "\<\%(and\|break\|in\|next\|not\|or\|redo\|rescue\|retry\|return\)\>[?!]\@!"
syn match   helionOperator       "\<defined?" display
syn match   helionKeyword	       "\<\%(super\|yield\|new\|let\)\>[?!]\@!"
syn match   helionBoolean	       "\<\%(true\|false\)\>[?!]\@!"
syn match   helionPseudoVariable "\<\%(nil\|self\)\>[?!]\@!" " TODO: reorganise

syn match   helionKeyword	       "\<\%(primitive\|yield\)\>[?!]\@!"

" Expensive Mode {{{1
" Match 'end' with the appropriate opening keyword for syntax based folding
" and special highlighting of module/class/method definitions
if !exists("b:helion_no_expensive") && !exists("helion_no_expensive")
  syn match  helionDefine "\<alias\>"  nextgroup=helionAliasDeclaration  skipwhite skipnl
  syn match  helionDefine "\<def\>"    nextgroup=helionMethodDeclaration skipwhite skipnl
  syn match  helionClass	"\<type\>"  nextgroup=helionClassDeclaration  skipwhite skipnl
  syn match  helionModule "\<module\>" nextgroup=helionModuleDeclaration skipwhite skipnl

  if s:foldable('def')
    syn region helionMethodBlock start="\<def\>"	matchgroup=helionDefine end="\%(\<def\_s\+\)\@<!\<end\>" contains=ALLBUT,@helionNotTop fold
  else
    syn region helionMethodBlock start="\<def\>"	matchgroup=helionDefine end="\%(\<def\_s\+\)\@<!\<end\>" contains=ALLBUT,@helionNotTop
  endif

  if s:foldable('type')
    syn region helionBlock start="\<type\>"	matchgroup=helionClass end="\<end\>" contains=ALLBUT,@helionNotTop fold
  else
    syn region helionBlock start="\<type\>"	matchgroup=helionClass end="\<end\>" contains=ALLBUT,@helionNotTop
  endif

  if s:foldable('module')
    syn region helionBlock start="\<module\>" matchgroup=helionModule end="\<end\>" contains=ALLBUT,@helionNotTop fold
  else
    syn region helionBlock start="\<module\>" matchgroup=helionModule end="\<end\>" contains=ALLBUT,@helionNotTop
  endif

  " modifiers
  syn match helionLineContinuation    "\\$" nextgroup=helionConditionalModifier,helionRepeatModifier skipwhite skipnl
  syn match helionConditionalModifier "\<\%(if\|unless\)\>"
  syn match helionRepeatModifier	    "\<\%(while\|until\)\>"

  if s:foldable('do')
    syn region helionDoBlock matchgroup=helionControl start="\<do\>" end="\<end\>" contains=ALLBUT,@helionNotTop fold
  else
    syn region helionDoBlock matchgroup=helionControl start="\<do\>" end="\<end\>" contains=ALLBUT,@helionNotTop
  endif

  " curly bracket block or hash literal
  if s:foldable('{')
    syn region helionCurlyBlock matchgroup=helionCurlyBlockDelimiter start="{" end="}" contains=ALLBUT,@helionNotTop fold
  else
    syn region helionCurlyBlock matchgroup=helionCurlyBlockDelimiter start="{" end="}" contains=ALLBUT,@helionNotTop
  endif

  if s:foldable('[')
    syn region helionArrayLiteral	matchgroup=helionArrayDelimiter start="\%(\w\|[\]})]\)\@<!\[" end="]" contains=ALLBUT,@helionNotTop fold
  endif

  " statements without 'do'
  if s:foldable('begin')
    syn region helionBlockExpression matchgroup=helionControl start="\<begin\>" end="\<end\>" contains=ALLBUT,@helionNotTop fold
  else
    syn region helionBlockExpression matchgroup=helionControl start="\<begin\>" end="\<end\>" contains=ALLBUT,@helionNotTop
  endif

  if s:foldable('case')
    syn region helionCaseExpression matchgroup=helionConditional start="\<case\>" end="\<end\>" contains=ALLBUT,@helionNotTop fold
  else
    syn region helionCaseExpression matchgroup=helionConditional start="\<case\>" end="\<end\>" contains=ALLBUT,@helionNotTop
  endif

  if s:foldable('if')
    " syn region helionConditionalExpression matchgroup=helionConditional start="\%(\%(^\|\.\.\.\=\|[{:,;([<>~\*%&^|+=-]\|\%(\<[_[:lower:]][_[:alnum:]]*\)\@<![?!]\)\s*\)\@<=\%(if\|unless\)\>" end="\%(\%(\%(\.\@1<!\.\)\|::\)\s*\)\@<!\<end\>" contains=ALLBUT,@helionNotTop fold
  else
  endif
    syn region helionConditionalExpression matchgroup=helionConditional start="\%(\%(^\|\.\.\.\=\|[{:,;([<>~\*%&^|+=-]\|\%(\<[_[:lower:]][_[:alnum:]]*\)\@<![?!]\)\s*\)\@<=\%(if\|unless\)\>" end="\%(\%(\%(\.\@1<!\.\)\|::\)\s*\)\@<!\<end\>" contains=ALLBUT,@helionNotTop

  syn match helionConditional "\<\%(then\|else\|when\)\>[?!]\@!"	contained containedin=helionCaseExpression
  syn match helionConditional "\<\%(then\|else\)\>[?!]\@!" contained containedin=helionConditionalExpression

  syn match helionExceptional	  "\<\%(\%(\%(;\|^\)\s*\)\@<=rescue\|else\|ensure\)\>[?!]\@!" contained containedin=helionBlockExpression
  syn match helionMethodExceptional "\<\%(\%(\%(;\|^\)\s*\)\@<=rescue\|else\|ensure\)\>[?!]\@!" contained containedin=helionMethodBlock

  " statements with optional 'do'
  syn region helionOptionalDoLine   matchgroup=helionRepeat start="\<for\>[?!]\@!" start="\%(\%(^\|\.\.\.\=\|[{:,;([<>~\*/%&^|+-]\|\%(\<[_[:lower:]][_[:alnum:]]*\)\@<![!=?]\)\s*\)\@<=\<\%(until\|while\)\>" matchgroup=helionOptionalDo end="\%(\<do\>\)" end="\ze\%(;\|$\)" oneline contains=ALLBUT,@helionNotTop

  if s:foldable('for')
    syn region helionRepeatExpression start="\<for\>[?!]\@!" start="\%(\%(^\|\.\.\.\=\|[{:,;([<>~\*/%&^|+-]\|\%(\<[_[:lower:]][_[:alnum:]]*\)\@<![!=?]\)\s*\)\@<=\<\%(until\|while\)\>" matchgroup=helionRepeat end="\<end\>" contains=ALLBUT,@helionNotTop nextgroup=helionOptionalDoLine fold
  else
    syn region helionRepeatExpression start="\<for\>[?!]\@!" start="\%(\%(^\|\.\.\.\=\|[{:,;([<>~\*/%&^|+-]\|\%(\<[_[:lower:]][_[:alnum:]]*\)\@<![!=?]\)\s*\)\@<=\<\%(until\|while\)\>" matchgroup=helionRepeat end="\<end\>" contains=ALLBUT,@helionNotTop nextgroup=helionOptionalDoLine
  endif

  if !exists("helion_minlines")
    let helion_minlines = 500
  endif
  exec "syn sync minlines=" . helion_minlines

else
  syn match helionControl "\<def\>[?!]\@!"    nextgroup=helionMethodDeclaration skipwhite skipnl
  syn match helionControl "\<type\>[?!]\@!"  nextgroup=helionClassDeclaration  skipwhite skipnl
  syn match helionControl "\<module\>[?!]\@!" nextgroup=helionModuleDeclaration skipwhite skipnl
  syn match helionControl "\<\%(case\|begin\|do\|for\|if\|unless\|while\|until\|else\|ensure\|then\|when\|end\)\>[?!]\@!"
endif

" Special Methods {{{1
if !exists("helion_no_special_methods")
  syn keyword helionAccess    public protected private public_class_method private_class_method public_constant private_constant module_function
  " attr is a common variable name
  syn match   helionAttribute "\%(\%(^\|;\)\s*\)\@<=attr\>\(\s*[.=]\)\@!"
  syn keyword helionAttribute attr_accessor attr_reader attr_writer
  syn match   helionControl   "\<\%(exit!\|\%(abort\|at_exit\|exit\|fork\|loop\|trap\)\>[?!]\@!\)"
  syn keyword helionEval	    eval class_eval instance_eval module_eval
  syn keyword helionException raise fail catch throw
  syn keyword helionInclude   use export
  syn keyword helionKeyword   callcc caller lambda proc
  " false positive with 'include?'
  syn match   helionMacro     "\<include\>[?!]\@!"
  syn keyword helionMacro     extends extend prepend refine using
  syn keyword helionMacro     alias_method define_method define_singleton_method remove_method undef_method
endif

" Comments and Documentation {{{1
syn match   helionSharpBang "\%^#!.*" display
syn keyword helionTodo	  FIXME NOTE TODO OPTIMIZE HACK REVIEW XXX todo contained
syn match   helionComment   "#.*" contains=helionSharpBang,helionSpaceError,helionTodo,@Spell
if !exists("helion_no_comment_fold") && s:foldable('#')
  syn region helionMultilineComment start="^\s*#.*\n\%(^\s*#\)\@=" end="^\s*#.*\n\%(^\s*#\)\@!" contains=helionComment transparent fold keepend
  syn region helionDocumentation	  start="^=begin\ze\%(\s.*\)\=$" end="^=end\%(\s.*\)\=$" contains=helionSpaceError,helionTodo,@Spell fold
else
  syn region helionDocumentation	  start="^=begin\s*$" end="^=end\s*$" contains=helionSpaceError,helionTodo,@Spell
endif

" Keyword Nobbling {{{1
" Note: this is a hack to prevent 'keywords' being highlighted as such when called as methods with an explicit receiver
syn match helionKeywordAsMethod "\%(\%(\.\@1<!\.\)\|::\)\_s*\%([_[:lower:]][_[:alnum:]]*\|\<\%(BEGIN\|END\)\>\)" transparent contains=NONE
syn match helionKeywordAsMethod "\(defined?\|exit!\)\@!\<[_[:lower:]][_[:alnum:]]*[?!]"			       transparent contains=NONE

" More Symbols {{{1
syn match  helionSymbol		"\%([{(,]\_s*\)\zs\l\w*[!?]\=::\@!"he=e-1
syn match  helionSymbol		"[]})\"':]\@1<!\<\%(\h\|[^\x00-\x7F]\)\%(\w\|[^\x00-\x7F]\)*[!?]\=:[[:space:],]\@="he=e-1
syn match  helionSymbol		"\%([{(,]\_s*\)\zs[[:space:],{]\l\w*[!?]\=::\@!"hs=s+1,he=e-1
syn match  helionSymbol		"[[:space:],{(]\%(\h\|[^\x00-\x7F]\)\%(\w\|[^\x00-\x7F]\)*[!?]\=:[[:space:],]\@="hs=s+1,he=e-1



syn match   helionSymbol        "\<\%(const\|some\|global\)\>[?!]\@!"


" __END__ Directive {{{1
if s:foldable('__END__')
  syn region helionData matchgroup=helionDataDirective start="^__END__$" end="\%$" fold
else
  syn region helionData matchgroup=helionDataDirective start="^__END__$" end="\%$"
endif

" Default Highlighting {{{1
hi def link helionClass			helionDefine
hi def link helionModule			helionDefine
hi def link helionMethodExceptional	helionDefine
hi def link helionDefine			Define
hi def link helionAccess			helionMacro
hi def link helionAttribute		helionMacro
hi def link helionMacro			Macro
hi def link helionMethodName		helionFunction
hi def link helionFunction		Function
hi def link helionConditional		Conditional
hi def link helionConditionalModifier	helionConditional
hi def link helionExceptional		helionConditional
hi def link helionRepeat			Repeat
hi def link helionRepeatModifier		helionRepeat
hi def link helionOptionalDo		helionRepeat
hi def link helionControl			Statement
hi def link helionInclude			Include
hi def link helionInteger			Number
hi def link helionASCIICode		Character
hi def link helionFloat			Float
hi def link helionBoolean			Boolean
hi def link helionException		Exception
if !exists("helion_no_identifiers")
  hi def link helionIdentifier		Identifier
else
  hi def link helionIdentifier		NONE
endif
hi def link helionClassVariable		helionIdentifier
hi def link helionConstant		Type
hi def link helionClassName		helionConstant
hi def link helionModuleName		helionConstant
hi def link helionGlobalVariable		helionIdentifier
hi def link helionInstanceVariable	helionIdentifier
hi def link helionPredefinedIdentifier	helionIdentifier
hi def link helionPredefinedConstant	helionPredefinedIdentifier
hi def link helionPredefinedVariable	helionPredefinedIdentifier
hi def link helionSymbol			Constant
hi def link helionKeyword			Keyword
hi def link helionOperator		Operator
hi def link helionBeginEnd		Statement
hi def link helionEval			Statement
hi def link helionPseudoVariable		Constant
hi def link helionCapitalizedMethod	helionLocalVariableOrMethod

hi def link helionComment			Comment
hi def link helionData			Comment
hi def link helionDataDirective		Delimiter
hi def link helionDocumentation		Comment
hi def link helionTodo			Todo

hi def link helionQuoteEscape		helionStringEscape
hi def link helionStringEscape		Special
hi def link helionInterpolationDelimiter	Delimiter
hi def link helionNoInterpolation		helionString
hi def link helionSharpBang		PreProc
hi def link helionRegexpDelimiter		helionStringDelimiter
hi def link helionSymbolDelimiter		helionSymbol
hi def link helionStringDelimiter		Delimiter
hi def link helionHeredoc			helionString
hi def link helionString			String
hi def link helionRegexpEscape		helionRegexpSpecial
hi def link helionRegexpQuantifier	helionRegexpSpecial
hi def link helionRegexpAnchor		helionRegexpSpecial
hi def link helionRegexpDot		helionRegexpCharClass
hi def link helionRegexpCharClass		helionRegexpSpecial
hi def link helionRegexpSpecial		Special
hi def link helionRegexpComment		Comment
hi def link helionRegexp			helionString

hi def link helionInvalidVariable		Error
hi def link helionError			Error
hi def link helionSpaceError		helionError

" Postscript {{{1
let b:current_syntax = "helion"

let &cpo = s:cpo_sav
unlet! s:cpo_sav

" vim: nowrap sw=2 sts=2 ts=8 noet fdm=marker:
