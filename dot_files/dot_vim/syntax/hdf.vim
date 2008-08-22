" Vim syntax file
" Language:	clearsilver templates (hdf files)
" Maintainer:   Culley Harrelson culley@fastmail.fm	
" Last Change:  2004-10-08 
" Filenames:    *.cs *.cs
" See:          http://www.clearsilver.net/

if !exists("main_syntax")
    if exists("b:current_syntax")
        finish
    endif
    let main_syntax = 'hdf'
endif

" lang flag
syn case ignore
syn match hdfLang "\[Lang\]" contained
highlight link hdfLang Identifier 

" Parameters
syn match   hdfParams    ".*="me=e-1 contains=HdfLang
syn match   hdfParams    ".*{"me=e-1 contains=HdfLang
syn match   hdfParams    ".*<<"me=e-2 contains=HdfLang
highlight link hdfParams Keyword

" Values
syn match   hdfValues    "=.*"hs=s+1
highlight link hdfValues String

syn region hdfHereDoc matchgroup=hdfHereEnd start="<<\z(\S*\)" end="^\z1$"
highlight link hdfHereDoc String
highlight link hdfHereEnd Identifier 

let b:current_syntax = "hdf"

if main_syntax == 'hdf'
  unlet main_syntax
endif
" vim: ts=8
