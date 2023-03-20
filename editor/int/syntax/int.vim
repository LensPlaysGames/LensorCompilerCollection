if exists('b:current_syntax')
  finish
endif

syntax keyword intTodo contained TODO FIXME XXX NOTE

syntax region intCommentLine start=";" end="$" display contains=intTodo

syntax match intSpecial contained "\\[nr\\\"\'t]" display

syntax region intString start="\"" skip="\\\"" end="\"" contains=intSpecial

syntax keyword intPrimitiveTypes
  \ integer
  \ void

syntax keyword intKeywords
  \ if
  \ else
  \ ext

syntax match intOperators "?\|+\|-\|\*\|:\|,\|<\|>\|&\||\|!\|\~\|%\|=\|\.\|/\(/\|*\)\@!"

syntax match intNumber "\v<\d+>"

highlight default link intPrimitiveTypes Type
highlight default link intKeywords       Keyword
highlight default link intNumber         Number
highlight default link intCommentLine    Comment
highlight default link intOperators      Operator
highlight default link intSpecial        Special
highlight default link intString         String

let b:current_syntax = 'int'
