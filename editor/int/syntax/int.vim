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

syn keyword intFunctionAttributes discardable nomangle inline noinline noreturn __noopt__ const pure used

syn keyword intTypeAttributes alignas

syn match intOperators "?\|+\|-\|\*\|:\|,\|<\|>\|&\||\|!\|\~\|%\|=\|\.\|/\(/\|*\)\@!"

syn match intNumber "\v<\d+>"

syn keyword intStatement return

syn keyword intBuiltinFunc __builtin_syscall __builtin_line __builtin_filename __builtin_debugtrap __builtin_inline

syn keyword intImport import nextgroup=intModuleName skipwhite
syn keyword intModule module nextgroup=intModuleName skipwhite
syn keyword intExportSymbol export

" NOTE: Include semicolon at the end of line?
syn match intModuleName "\<[a-zA-Z0-9_]*" contained skipwhite

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
hi def link intStatement          Statement
hi def link intModule             Special
hi def link intImport             Special
hi def link intExportSymbol       Special
hi def link intModuleName         Function
hi def link intBuiltinFunc        Identifier

let b:current_syntax = 'int'
