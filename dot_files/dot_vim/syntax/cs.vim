" Vim syntax file
" Language:	clearsilver templates (cs files)
" Maintainer:   Culley Harrelson culley@fastmail.fm	
" Last Change:  2004-10-08 
" Filenames:    *.cs *.cs
" See:          http://www.clearsilver.net/

if !exists("main_syntax")
  if version < 600
    syntax clear
  elseif exists("b:current_syntax")
  finish
endif
  let main_syntax = 'cs'
endif

syn case ignore

runtime! syntax/html.vim

syn region csRegion matchgroup=Delimiter start="<?cs" end="?>" contains=csInclude,htmlString,csLabel,csRepeat,csConditional,csFunction,csOperator contained
syn keyword csInclude	include linclude contained
syn keyword csLabel var evar lvar set name contained
syn keyword csRepeat each loop with contained
syn keyword csFunction def call contained
syn keyword csConditional if else elif alt contained

syn match csOperator	"[|&=!?:><+-/*%.\[\]#\$]"	contained 

syn cluster htmlPreproc add=csRegion

if version >= 508 || !exists("did_cs_syn_inits")
  if version < 508
    let did_cs_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink csInclude	Include
  HiLink csLabel	Label
  HiLink csRepeat	Repeat
  HiLink csConditional	Conditional
  HiLink csFunction	Function
  HiLink csOperator	Operator
  delcommand HiLink
endif

let b:current_syntax = "cs"

if main_syntax == 'cs'
  unlet main_syntax
endif

" vim: ts=8
