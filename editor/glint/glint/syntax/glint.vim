if exists('b:current_syntax')
  finish
endif

syn keyword glintTodo contained TODO FIXME XXX NOTE

syn region glintCommentLine start=";;" end="$" display contains=glintTodo

syn region glintRawString start="'" end="'" display

syn match glintSpecial contained "\\[nr\\\"\'t]" display
syn region glintString start="\"" skip="\\\"" end="\"" contains=glintSpecial

syn keyword glintFFITypes
  \ cshort
  \ cushort
  \ csize
  \ cusize
  \ cint
  \ cuint
  \ clong
  \ culong
  \ clonglong
  \ culonglong
  \ void

syn keyword glintPrimitiveTypes
  \ Bool
  \ Boolean
  \ Byte
  \ bool
  \ boolean
  \ int
  \ uint
  \ void

syn keyword glintDataTypes
  \ struct
  \ enum
  \ sum

syn keyword glintKeywords
  \ alignof
  \ external
  \ return
  \ sizeof
  \ match
  \ print
  \ in
  \ mapf

syn keyword glintRepeat
  \ for
  \ cfor
  \ while

syn keyword glintConditional
  \ if
  \ else

syn keyword glintMacro macro emits endmacro
syn match glintMacroArgs '\$[a-zA-Z_][a-zA-Z0-9_]*'

syn keyword glintFunctionAttributes
  \ discardable
  \ nomangle
  \ inline
  \ noinline
  \ noreturn
  \ __noopt__
  \ const
  \ pure
  \ used
  \ flatten

syn keyword glintTypeAttributes alignas

" syn match glintOperators "?\|+\|-\|\*\|:\|,\|<\|>\|&\||\|!\|\~\|%\|=\|\.\|/\(/\|*\)\@!"
syn match glintOperators "[/*+-\%~&|^<>]="
syn keyword glintOperators
  \ @
  \ !
  \ +
  \ -
  \ *
  \ /
  \ &
  \ =
  \ <
  \ >
  \ bitand
  \ bitor
  \ bitxor
  \ ::
  \ :
  \ ++
  \ --

syn match glintNumber "\v<\d+>"

syn keyword glintBuiltinFunc __builtin_syscall __builtin_line __builtin_filename __builtin_debugtrap __builtin_inline

syn keyword glintImport import nextgroup=glintModuleName skipwhite
syn keyword glintModule module nextgroup=glintModuleName skipwhite
syn keyword glintExportSymbol export
syn keyword glintSupplant supplant

" NOTE: Include semicolon at the end of line?
syn match glintModuleName "\<[a-zA-Z0-9_]*" contained skipwhite

hi def link glintFFITypes           Type
hi def link glintPrimitiveTypes     Type
hi def link glintKeywords           Keyword
hi def link glintRepeat             Repeat
hi def link glintConditional        Conditional
hi def link glintNumber             Number
hi def link glintCommentLine        Comment
hi def link glintOperators          Operator
hi def link glintSpecial            Special
hi def link glintRawString          String
hi def link glintString             String
hi def link glintFunctionAttributes StorageClass
hi def link glintTypeAttributes     StorageClass
hi def link glintMacro              Macro
hi def link glintMacroArgs          Function
hi def link glintModule             Special
hi def link glintImport             Special
hi def link glintExportSymbol       StorageClass
hi def link glintModuleName         Function
hi def link glintBuiltinFunc        Identifier
hi def link glintDataTypes          Structure
hi def link glintSupplant           StorageClass

let b:current_syntax = 'glint'

" vim:sw=2 ts=2:
