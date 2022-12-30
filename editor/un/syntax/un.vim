if exists('b:current_syntax')
  finish
endif

syntax region unCommentLine1 start=";" end="$"
syntax region unCommentLine2 start="#" end="$"

syntax keyword unPrimitiveTypes
  \ integer
  \ void

syntax keyword unKeywords
  \ if
  \ else
  \ ext

syntax match unOperators "?\|+\|-\|\*\|;\|:\|,\|<\|>\|&\||\|!\|\~\|%\|=\|\.\|/\(/\|*\)\@!"

syntax match unNumber "\v<\d+>"


highlight default link unPrimitiveTypes Type
highlight default link unKeywords       Keyword
highlight default link unNumber         Number
highlight default link unCommentLine1   Comment
highlight default link unCommentLine2   Comment
highlight default link unOperators      Operator

let b:current_syntax = 'un'
