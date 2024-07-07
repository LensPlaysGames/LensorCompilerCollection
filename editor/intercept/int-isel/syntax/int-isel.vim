if exists('b:current_syntax')
    finish
endif

syn keyword intIselTodo contained TODO FIXME XXX NOTE

syn region intIselCommentLine start=";;" end="$" contains=intIselTodo

syn keyword intIselKeyword match emit discard is

syn keyword intIselBuiltinTypes Immediate IMM Register REG Name Block Function Local Static

syn match intIselOperator '='
syn match intIselOperator '!'
syn match intIselOperator '\~'
syn match intIselOperator '@'
syn match intIselOperator '&'

syn region intIselString start='"' skip='\\"' end='"'

syn match intIselNumber "\v<\d+>"

hi def link intIselTodo         Todo
hi def link intIselCommentLine  Comment
hi def link intIselKeyword      Keyword
hi def link intIselBuiltinTypes Type
hi def link intIselOperator     Operator
hi def link intIselString       String
hi def link intIselNumber       Number

let b:current_syntax = 'int-isel'
