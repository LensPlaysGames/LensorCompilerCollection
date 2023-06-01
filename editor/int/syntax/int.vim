if exists('b:current_syntax')
  finish
endif

syn keyword intTodo contained TODO FIXME XXX NOTE

syn region intCommentLine start=";" end="$" display contains=intTodo

syn match intSpecial contained "\\[nr\\\"\'t]" display

syn region intString start="\"" skip="\\\"" end="\"" contains=intSpecial

syn keyword intPrimitiveTypes
  \ integer
  \ void
  \ byte

syn keyword intKeywords
  \ ext
  \ type

syn keyword intRepeat
  \ for
  \ while

syn keyword intConditional
  \ if
  \ else

syn keyword intMacro macro emits endmacro
syn match intMacroArgs '\$[a-zA-Z_][a-zA-Z0-9_]*'

syn keyword intFunctionAttributes discardable nomangle

syn keyword intTypeAttributes alignas

syn match intOperators "?\|+\|-\|\*\|:\|,\|<\|>\|&\||\|!\|\~\|%\|=\|\.\|/\(/\|*\)\@!"

syn match intNumber "\v<\d+>"

hi def link intPrimitiveTypes     Type
hi def link intKeywords           Keyword
hi def link intRepeat             Repeat
hi def link intConditional        Conditional
hi def link intNumber             Number
hi def link intCommentLine        Comment
hi def link intOperators          Operator
hi def link intSpecial            Special
hi def link intString             String
hi def link intFunctionAttributes StorageClass
hi def link intTypeAttributes     StorageClass
hi def link intMacro              Macro
hi def link intMacroArgs          Function

let b:current_syntax = 'int'
