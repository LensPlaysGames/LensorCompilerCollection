if exists('b:current_syntax')
  finish
endif

syntax region intCommentLine1 start=";" end="$"

syntax keyword intPrimitiveTypes
  \ integer
  \ void

syntax keyword intKeywords
  \ if
  \ else
  \ ext

syntax match intOperators "?\|+\|-\|\*\|;\|:\|,\|<\|>\|&\||\|!\|\~\|%\|=\|\.\|/\(/\|*\)\@!"

syntax match intNumber "\v<\d+>"


highlight default link intPrimitiveTypes Type
highlight default link intKeywords       Keyword
highlight default link intNumber         Number
highlight default link intCommentLine1   Comment
highlight default link intCommentLine2   Comment
highlight default link intOperators      Operator

let b:current_syntax = 'int'
