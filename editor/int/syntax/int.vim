if exists('b:current_syntax')
  finish
endif

syntax keyword intTodo contained TODO FIXME XXX NOTE

syntax region intCommentLine start=";" end="$" display contains=intTodo

syntax match intSpecial contained "\\[nr\\\"\'t]" display

syntax match intFormat contained "%[csSCdiuDIUZzxXpbTF%m]" display
" TODO: Not sure about these
" syntax match intFormat contained "%\d" display
" syntax match intFormat contained "%\d\d" display
" syntax match intFormat contained "%\d\d\d" display

syntax region intString start="\"" skip="\\\"" end="\"" contains=intSpecial,intFormat

syntax keyword intPrimitiveTypes
  \ integer
  \ void
  \ byte

syntax keyword intKeywords
  \ ext
  \ type
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
highlight default link intFormat         Special
highlight default link intString         String

let b:current_syntax = 'int'
