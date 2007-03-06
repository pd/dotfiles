" minimalist syntax highlighting for p9's rc.
" WTFPL.
if version < 600
	syntax clear
elseif exists("b:current_syntax")
	finish
endif

syn keyword rcKeyword if for while switch case fn
syn match   rcString "'[^']*'"
syn match   rcComment "\#.*"

syn cluster rcAll contains=rcList,rcKeyword,rcString,rcParenError

hi def link rcKeyword statement
hi def link rcString  String
hi def link rcComment Comment

let b:current_syntax = "rc"
